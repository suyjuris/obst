
// Written by Philipp Czerner, 2018. Public Domain.
// See LICENSE.md for license information.

// This file contains platform-specific code for Linux, responsible for initialising a window and 3D
// rendering context, as well as drawing a UI.

#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/extensions/Xrandr.h>
#include <GL/glx.h>
#include <time.h>
#include <locale.h>
#include <sys/stat.h>

typedef GLXContext (*glXCreateContextAttribsARB_t) (
    Display *dpy, GLXFBConfig config, GLXContext share_context, Bool direct, const int *attrib_list
);

#define OBST_PLATFORM_LINUX

#include "global.hpp"
#include "platform.hpp"
#include "platform_linux_autogen.cpp"
#include "obst.cpp"

typedef u8  stbtt_uint8;
typedef s8  stbtt_int8;
typedef u16 stbtt_uint16;
typedef s16 stbtt_int16;
typedef u32 stbtt_uint32;
typedef s32 stbtt_int32;
#define STBTT_assert(x) assert(x)

#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"

Array_t<u8> array_load_from_file(Array_t<u8> path) {
    assert(*path.end() == 0);
    FILE* f = fopen((char*)path.data, "rb");

    if (f == nullptr) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (1)\n", path.data);
        perror("Error"); exit(1025);
    }

    if (fseek(f, 0, SEEK_END) == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (2)\n", path.data);
        perror("Error"); exit(1026);
    }

    s64 f_size = ftell(f);
    if (f_size == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (3)\n", path.data);
        perror("Error"); exit(1027);
    }

    if (fseek(f, 0, SEEK_SET) == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (4)\n", path.data);
        perror("Error"); exit(1028);
    }

    Array_t<u8> data = array_create<u8>(f_size);
    s64 read = fread(data.data, 1, data.size, f);
    if (read < f_size) {
        if (ferror(f)) {
            fprintf(stderr, "Error: Could not open file '%s' for reading (5)\n", path.data);
            perror("Error"); exit(1029);
        } else {
            fprintf(stderr, "Error: Could not open file '%s' for reading (6)\n", path.data);
            fprintf(stderr, "Error: Unexpected eof while reading file. (Concurrent modification?)\n");
            exit(1030);
        }
    }

    if (fclose(f)) {
        // Something weird is happening here, but hey, we already have our data.
        fprintf(stderr, "Warning: Could not close file '%s'\n", path.data);
        perror("Warning:");
    }

    return data;
}
Array_t<u8> array_load_from_file(char const* path) {
    return array_load_from_file({(u8*)path, (s64)strlen(path)});
}

struct Text_box_lookup {
    u64 hash = 0; s64 index = -1;
};

struct Undo_item {
    enum Type: u8 {
        NONE, INSERT, DELETE
    };
    enum Flags: u8 {
        COMMITTED = 1, REVERSED = 2, GROUPED = 4
    };
    u8 type = 0;
    u8 flags = 0;
    union {
        struct { s64 ins_offset, ins_beg; };
        struct { s64 del_beg, del_end; };
    };

    Undo_item(u8 type = 0, u8 flags = 0): type{type}, flags{flags}, ins_offset{}, ins_beg{} {}
};

struct Text_entry {
    Array_dyn<u8> text;
    s64 cursor, cursor_row, cursor_col;
    s64 offset_x, offset_y;
    s64 draw_w, draw_h;
    s64 slot;
    s64 selection, selection_row, selection_col;
    bool cursor_draw, disable_newline;
    Array_dyn<Undo_item> undo_stack, redo_stack;
    Array_dyn<u8> undo_data, redo_data;
};

struct Scrollbar {
    s64 offset = 0;
    s64 total_height = 0;
    s64 slot;
    s64 slot_area;
    s64 drag_offset, drag_y;

    Animation anim;
};

struct Resizer {
    s64 size = 0, size_min;
    s64 drag_size, drag_y;
    s64 slot;
};

struct Rect {
    s64 x = 0, y = 0, w = 0, h = 0;
};

struct Padding {
    s64 pad_x = 0, pad_y = 0, mar_x = 0, mar_y = 0;
};

// Data for the current status of text preparation, i.e. the packing of rasterised glyphs into a
// texture. Initialised in lui_text_prepare_init.
struct Text_preparation {
    Array_t<u8> image; // The current content of the font texture
    s64 size = 0; // Size of the font texture
    s64 x = 0, y = 0; // The next rectangle begins here (+1 padding)
    s64 y_incr = 0; // How far to move y when moving into the next line
    Array_dyn<int> glyph_buf; // Temporary storage to hold the glyphs
    Array_dyn<u8>  image_buf; // Temporary storage to hold pixel data
    bool dirty = false; // Whether we need to re-send the texture to the GPU
    Array_dyn<Text_box> cache; // Holds a copy of everything we have already rendered
    Array_t<Text_box_lookup> cache_lookup; // Hash table for the cache
};

// Keeps the necessary data to manage OpenGL and other data for the uil layer
struct Lui_context {
    // Indices for all the vertex attributes
    enum Attributes: GLuint {
        UIRECT_POS = 0, UIRECT_FILL, UIRECT_ATTR_COUNT,
        UITEXT_POS = 0, UITEXT_TPOS, UITEXT_FILL, UITEXT_ATTR_COUNT,
        UIBUTTON_POS = 0, UIBUTTON_X, UIBUTTON_SIZE, UIBUTTON_BORDER, UIBUTTON_COLOR, UIBUTTON_GRAD, UIBUTTON_CIRCLE, UIBUTTON_ATTR_COUNT,
    };
    // Names for all uniforms
    enum Uniforms: int {
        UIRECT_ORIGIN, UIRECT_SCALE,
        UITEXT_ORIGIN, UITEXT_SCALE, UITEXT_SAMPLER,
        UIBUTTON_ORIGIN, UIBUTTON_SCALE,
        UNIFORMS_COUNT
    };
    
    // Ids of the shader programs
    GLuint program_uirect;
    GLuint program_uitext;
    GLuint program_uibutton;
    // Ids of the uniforms
    Array_t<GLint> uniforms;

    GLuint uitext_tex = 0;
    Array_dyn<Text_box> uitext_boxes;

    // Buffers for the vertex attribute arrays    
    Array_dyn<float> buf_uirect_pos;
    Array_dyn<u8>    buf_uirect_fill;

    Array_dyn<float> buf_uitext_pos;
    Array_dyn<float> buf_uitext_tpos;
    Array_dyn<u8>    buf_uitext_fill;

    Array_dyn<float> buf_uibutton_pos;
    Array_dyn<float> buf_uibutton_x;
    Array_dyn<float> buf_uibutton_size;
    Array_dyn<float> buf_uibutton_border;
    Array_dyn<float> buf_uibutton_color;
    Array_dyn<float> buf_uibutton_grad;
    Array_dyn<float> buf_uibutton_circle;

    // Names of the buffers for the vertex attributes of each shader
    Array_t<GLuint> buffers_uirect;
    Array_t<GLuint> buffers_uitext;
    Array_t<GLuint> buffers_uibutton;

    static constexpr float LAYER_BACK = 0.1f;
    static constexpr float LAYER_MIDDLE = 0.085f;
    static constexpr float LAYER_FRONT = 0.07f;

    // Data for font rendering
    struct Font_instance {
        s64 info_index;
        float scale, ascent, height, newline, space;
    };
    enum Font_instance_id: u8 {
        FONT_LUI_NORMAL, FONT_LUI_ITALIC, FONT_LUI_BOLD, FONT_LUI_HEADER, FONT_LUI_SMALL,
        FONT_LUI_SANS, FONT_LUI_BUTTON, FONT_BDD_NORMAL, FONT_BDD_ITALICS, FONT_BDD_SMALL,
        FONT_COUNT
    };

constexpr static u8 font_magic[] = {(u8)15, (u8)183, (u8)0, (u8)188, (u8)140, (u8)140, (u8)156, (u8)250, (u8)81, (u8)112, (u8)36, (u8)51, (u8)118, (u8)200, (u8)159, (u8)15};
    Array_t<u8*> font_files;
    Array_t<Array_t<u8>> font_file_data;
    Array_t<stbtt_fontinfo> font_info;
    Array_t<Font_instance> fonts;

    // Data for generating the texture containing the text (the text preparation)
    Text_preparation prep_ui, prep_bdd;
    
    // Additional slots for text belonging to the UI
    enum Fmt_slots_lui: s64 {
        SLOT_INITTEXT = Text_fmt::SLOT_PLATFORM_FIRST, SLOT_BUTTON_DESC_CREATE, SLOT_BUTTON_DESC_OP,
        SLOT_BUTTON_DESC_REMOVEALL, SLOT_BUTTON_DESC_HELP, SLOT_BUTTON_DESC_CONTEXT, SLOT_LABEL_BASE,
        SLOT_LABEL_BITORDER, SLOT_LABEL_VARORDER, SLOT_LABEL_FIRSTNODE, SLOT_LABEL_SECONDNODE, SLOT_LABEL_INPUT,
        SLOT_LABEL_NUMBERS, SLOT_LABEL_FORMULA, SLOT_RESIZER_CREATE, SLOT_ENTRY_NUMBERS, SLOT_ENTRY_FORMULA,
        SLOT_ENTRY_BASE, SLOT_ENTRY_BITORDER, SLOT_ENTRY_VARORDER, SLOT_BUTTON_CREATE,
        SLOT_LABEL_UNION, SLOT_LABEL_INTERSECTION, SLOT_LABEL_COMPLEMENT, SLOT_ENTRY_FIRSTNODE,
        SLOT_ENTRY_SECONDNODE, SLOT_BUTTON_OP, SLOT_LABEL_OP_U, SLOT_LABEL_OP_I, SLOT_LABEL_OP_C,
        SLOT_LABEL_OPERATION, SLOT_BUTTON_REMOVEALL, SLOT_LABEL_HELP_SHOW, SLOT_LABEL_HELP_HIDE,
        SLOT_BUTTON_HELP, SLOT_BUTTON_PREV, SLOT_BUTTON_NEXT, SLOT_BUTTON_HELP_CLOSE, SLOT_SCROLL_PANEL,
        SLOT_HELPTEXT, SLOT_CANVAS, SLOT_PANEL,
        SLOT_COUNT
    };
    
    // Data for formatted text display
    u64 fmt_flags; // Current flags
    Array_dyn<Text_box> fmt_boxes; // Set of boxes we are currently generating
    Array_dyn<Array_dyn<Text_box>> fmt_slots; // Stored sets of boxes

    enum Element_flags: u16 {
        DRAW_FOCUSED = 1,
        DRAW_ACTIVE = 2,
        DRAW_PRESSED = 4,
        DRAW_DISABLED = 8,
        DRAW_BUTTON = 16,
        DRAW_ENTRY = 32,
        DRAW_RADIO = 64,
        DRAW_SCROLL = 128,
        DRAW_RESIZER = 256,
        DRAW_AREA = 512,
        DRAW_COMPACT = 1024,
    };

    enum Entry_names: u8 {
        ENTRY_NUMBERS, ENTRY_FORMULA, ENTRY_BASE, ENTRY_BITORDER, ENTRY_VARORDER, ENTRY_FIRSTNODE, ENTRY_SECONDNODE,
        ENTRY_COUNT
    };
    enum Resizer_names: u8 {
        RESIZER_CREATE, RESIZER_PANEL,
        RESIZER_COUNT
    };
    
    static constexpr double CURSOR_BLINK_DELAY = 0.5;
    
    // State of the ui elements
    Array_t<Text_entry> entries;
    Array_t<u64> elem_flags;
    Array_t<Rect> elem_bb;
    s64 elem_focused = 0;
    s64 elem_tabindex = 0;
    double cursor_next_blink = 0.f;
    bool cursor_blinked = false;
    Padding padding_entry;
    Scrollbar scroll_panel;
    Array_t<Resizer> resizers;

    // Lengths used by multiple places
    //@Cleanup: Make this DPI aware
    s64 panel_left_width = 475;
    s64 width_scrollbar = 6;
    s64 width_resizer = 8;
    s64 width_button_max = 40;

    // Input state
    s64 pointer_x, pointer_y;
    Array_dyn<Key> input_queue;
    s64 drag_el = -1;
    bool window_has_focus = false;

    // Main loop flag
    bool main_loop_active = false;

    // State for bddinfo
    bool bddinfo_active = false;
    s64 bddinfo_x, bddinfo_y, bddinfo_pad; // pixels relative to canvas

    // State for helptext
    bool helptext_active = false;
};

constexpr u8 Lui_context::font_magic[];

namespace Platform_clipboard {
enum Types: u8 {
    CONTROL_C, MIDDLE_BUTTON, COUNT
};
}

namespace Cursor_type {
enum Cursor_type: u8 {
    NORMAL, TEXT, RESIZE
};
}

struct Platform_state {
    Display* display = nullptr;
    Window window;
    GLXWindow window_glx;
    Atom sel_primary, sel_clipboard, sel_target, sel_utf8str, sel_string, sel_incr;

    u8 cursor_type = Cursor_type::NORMAL;
    Cursor cursor_text, cursor_resize;

    Lui_context gl_context; //@Cleanup: Rename to lui_context

    double redraw_next = -1;
    double redraw_last = 0.f;
    s64 rate = -1;

    Array_dyn<u8> clipboard_recv;
    Array_dyn<u8> clipboard_send[Platform_clipboard::COUNT] = {};
};
Platform_state global_platform;

void platform_ui_bddinfo_show(float x, float y, float pad) {
    Lui_context* context = &global_platform.gl_context;
    context->bddinfo_active = true;
    context->bddinfo_x   =  (s64)std::round((x - global_context.origin_x) * global_context.scale);
    context->bddinfo_y   = -(s64)std::round((y - global_context.origin_y) * global_context.scale) + global_context.height;
    context->bddinfo_pad =  (s64)std::round( pad                          * global_context.scale);
}
void platform_ui_bddinfo_hide() {
    global_platform.gl_context.bddinfo_active = false;
}

void platform_main_loop_active(bool active) {
    global_platform.gl_context.main_loop_active = active;
}

void platform_redraw(double t) {
    global_platform.redraw_next = std::min(global_platform.redraw_next, t);
}

double platform_now() {
    timespec t;
    clock_gettime(CLOCK_MONOTONIC_RAW, &t);
    return (double)t.tv_sec + (double)t.tv_nsec * 1e-9;
}

void _platform_handle_resize(s64 width, s64 height) {
    global_context.screen_w = width;
    global_context.screen_h = height;
    
    global_context.width = std::max(global_context.screen_w - global_platform.gl_context.panel_left_width, 400ll);
    global_context.height = global_context.screen_h;
    global_context.canvas_x = global_platform.gl_context.panel_left_width;
    global_context.canvas_y = 0;

    glViewport(0.0, 0.0, global_context.screen_w, global_context.screen_h);
    
    application_handle_resize();
}

void _platform_init_font(s64 font_style, s64 index, float size) {
    Lui_context* context = &global_platform.gl_context;

    if (index == -1) index = context->fonts[font_style].info_index;
    
    Lui_context::Font_instance inst;
    inst.info_index = index;
    inst.scale = stbtt_ScaleForPixelHeight(&context->font_info[index], size);
    
    {int ascent, descent, linegap;
    stbtt_GetFontVMetrics(&context->font_info[index], &ascent, &descent, &linegap);
    inst.ascent = (float)ascent * inst.scale;
    inst.height = (float)(ascent - descent) * inst.scale;
    inst.newline = (float)(ascent - descent + linegap) * inst.scale;}

    {int advance;
    stbtt_GetCodepointHMetrics(&context->font_info[index], ' ', &advance, nullptr);
    inst.space = (float)advance * inst.scale;}

    context->fonts[font_style] = inst;
}

void _platform_init_gl(Platform_state* platform) {
    assert(platform);
    _platform_init_gl_pointers();

    // WebGL has no vertex array objects, OpenGL requires them. Yay. However, creating a single one
    // is fine, and we can do everything else WebGL style.
    GLuint vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);

    // This loads all the application shaders
    webgl_init(&global_context);

    // Now we load our own shaders
    
    GLbyte shader_v_uirect[] = 
        "attribute vec3 pos;\n"
        "attribute vec4 fill;\n"
        "varying vec4 v_fill;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "void main() {\n"
        "    gl_Position = vec4((pos.xy - origin)*scale, vec2(pos.z, 1));\n"
        "    v_fill = fill;\n"
        "}\n";

    GLbyte shader_f_uirect[] =
        "varying vec4 v_fill;\n"
        "void main() {\n"
        "    gl_FragColor = v_fill;\n"
        "}\n";
    
    GLbyte shader_v_uitext[] =
        "attribute vec2 pos;\n"
        "attribute vec2 tpos;\n"
        "attribute vec4 fill;\n"
        "varying vec2 v_tpos;\n"
        "varying vec4 v_fill;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "void main() {\n"
        "    gl_Position = vec4((pos - origin)*scale, 0.08, 1);\n"
        "    v_tpos = tpos;\n"
        "    v_fill = fill;\n"
        "}\n";

    GLbyte shader_f_uitext[] =
        "varying vec2 v_tpos;\n"
        "varying vec4 v_fill;\n"
        "uniform sampler2D sampler;\n"
        "void main() {\n"
        "    vec4 col = v_fill;\n"
        "    col.a *= texture2D(sampler, v_tpos).r;\n"
        "    gl_FragColor = col;\n"
        "}\n";

    GLbyte shader_v_uibutton[] =
        "attribute vec2 pos;\n"
        "attribute vec2 x;\n"
        "attribute float size;\n"
        "attribute vec3 border;\n"
        "attribute vec3 color;\n"
        "attribute vec4 grad;\n"
        "attribute vec2 circle;\n"
        "varying vec2 v_p;\n"
        "varying float v_size;\n"
        "varying vec3 v_border;\n"
        "varying vec3 v_color;\n"
        "varying vec4 v_grad;\n"
        "varying vec2 v_circle;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "void main() {\n"
        "    gl_Position = vec4((pos - origin)*scale, 0.09, 1);\n"
        "    v_p = x;\n"
        "    v_size = size;\n"
        "    v_border = border;\n"
        "    v_color = color;\n"
        "    v_grad = grad;\n"
        "    v_circle = circle;\n"
        "}\n";
    
    GLbyte shader_f_uibutton[] =
        "varying vec2 v_p;\n"
        "varying float v_size;\n"
        "varying vec3 v_border;\n"
        "varying vec3 v_color;\n"
        "varying vec4 v_grad;\n"
        "varying vec2 v_circle;\n"
        "void main() {\n"
        "    float dp = v_circle[0];\n"
        "    float f = pow(abs(v_p.x)/v_size, dp) + pow(abs(v_p.y)/v_size, dp) - 1.0;\n"
        "    vec2 df = vec2(dp/v_size*pow(abs(v_p.x)/v_size, dp-1.0), dp/v_size*pow(abs(v_p.y)/v_size, dp-1.0));\n"
        "    float d = f / length(df);\n"
        "    if (d > v_border[0] + 0.5) discard;\n"
        "    if (0.0 < d && d < 3.0) {\n"
        "        vec2 pp = v_p - f * df / dot(df, df) * vec2(sign(v_p.x), sign(v_p.y));\n"
        "        float f2 = pow(abs(pp.x)/v_size, dp) + pow(abs(pp.y)/v_size, dp) - 1.0;\n"
        "        vec2 df2 = vec2(dp/v_size*pow(abs(pp.x)/v_size, dp-1.0), dp/v_size*pow(abs(pp.y)/v_size, dp-1.0));\n"
        "        d += f2 / length(df2);\n"
        "    }\n"
        "    \n"
        "    float t = (v_p.y/v_size+1.0) / 2.0;\n"
        "    vec3 col1 = v_color * mix(v_border[1], v_border[2], clamp(t, 0.0, 1.0));\n"
        "    float l = t < v_grad[0] ? mix(v_grad[1], v_grad[2], t / v_grad[0])\n"
        "            : mix(v_grad[2], v_grad[3], clamp((t - v_grad[0]) / (1.0 - v_grad[0]), 0.0, 1.0));\n"
        "    vec3 col2 = vec3(l, l, l);\n"
        "    vec3 col3 = mix(col2, col1, clamp(d+0.5, 0.0, 1.0));\n"
        "    vec3 col4 = mix(col3*v_circle[1], col3, clamp(length(v_p) - v_size*0.5 + 0.5, 0.0, 1.0));\n"
        "    float a = 1.0 - max(0.0, d - v_border[0] + 0.5);\n"
        "    gl_FragColor = vec4(col4, a);\n"
        "}\n";

    GLuint program;
    auto context = &platform->gl_context; // The macros expect a local context variable
    context->uniforms = array_create<GLint>(context->UNIFORMS_COUNT);
    
    OBST_PROGRAM_INIT(uirect);
    OBST_ATTRIB(UIRECT_POS, pos);
    OBST_ATTRIB(UIRECT_FILL, fill);
    OBST_PROGRAM_LINK(uirect);
    OBST_UNIFORM(UIRECT_ORIGIN, origin);
    OBST_UNIFORM(UIRECT_SCALE, scale);

    OBST_GEN_BUFFERS(uirect, UIRECT);

    OBST_PROGRAM_INIT(uitext);
    OBST_ATTRIB(UITEXT_POS, pos);
    OBST_ATTRIB(UITEXT_TPOS, tpos);
    OBST_ATTRIB(UITEXT_FILL, fill);
    OBST_PROGRAM_LINK(uitext);
    OBST_UNIFORM(UITEXT_ORIGIN, origin);
    OBST_UNIFORM(UITEXT_SCALE, scale);
    OBST_UNIFORM(UITEXT_SAMPLER, sampler);

    OBST_GEN_BUFFERS(uitext, UITEXT);

    OBST_PROGRAM_INIT(uibutton);
    OBST_ATTRIB(UIBUTTON_POS, pos);
    OBST_ATTRIB(UIBUTTON_X, x);
    OBST_ATTRIB(UIBUTTON_SIZE, size);
    OBST_ATTRIB(UIBUTTON_BORDER, border);
    OBST_ATTRIB(UIBUTTON_COLOR, color);
    OBST_ATTRIB(UIBUTTON_GRAD, grad);
    OBST_ATTRIB(UIBUTTON_CIRCLE, circle);
    OBST_PROGRAM_LINK(uibutton);
    OBST_UNIFORM(UIBUTTON_ORIGIN, origin);
    OBST_UNIFORM(UIBUTTON_SCALE, scale);

    OBST_GEN_BUFFERS(uibutton, UIBUTTON);
    
    //@Cleanup: Check max size using RECTANGLE_TEXTURE_SIZE
}

void lui_draw_rect(Lui_context* context, s64 x, s64 y, s64 w, s64 h, float z, u8* fill) {
    float x1 = x,   y1 = y;
    float x2 = x+w, y2 = y+h;

    array_append(&context->buf_uirect_pos, {
        x1, y1, z, x2, y1, z, x2, y2, z, x1, y1, z, x2, y2, z, x1, y2, z
    });
    for (s64 i = 0; i < 6; ++i) {
        array_append(&context->buf_uirect_fill, {fill, 4});
    }
}
void lui_draw_rect(Lui_context* context, float x1, float y1, float w, float h, float z, u8* fill) {
    float x2 = x1+w, y2 = y1+h;
    
    array_append(&context->buf_uirect_pos, {
        x1, y1, z, x2, y1, z, x2, y2, z, x1, y1, z, x2, y2, z, x1, y2, z
    });
    for (s64 i = 0; i < 6; ++i) {
        array_append(&context->buf_uirect_fill, {fill, 4});
    }
}

void lui_text_prepare_init(Text_preparation* prep, s64 texture_size) {
    prep->image = array_create<u8>(texture_size * texture_size);
    prep->size = texture_size;
    prep->x = 0;
    prep->y = 0;
    prep->y_incr = 0;
    prep->dirty = false;
    prep->cache.size = 0;

    if (prep->cache_lookup.size != 4096) {
        array_free(&prep->cache_lookup);
        prep->cache_lookup = array_create<Text_box_lookup>(4096);
    }
    memset(prep->cache_lookup.data, -1, prep->cache_lookup.size * sizeof(Text_box_lookup));
}

void lui_text_prepare_word(Lui_context* context, Text_preparation* prep, u8 font, Array_t<u8> word, Text_box* box, float letter_fac=1.f) {
    assert(box);

    union {
        float letter_fac_;
        u32 letter_fac_u;
    };
    letter_fac_ = letter_fac;
    
    // Lookup in hash table
    u64 hash = 14695981039346656037ull ^ font ^ (word.size << 8) ^ ((u64)letter_fac_u << 32);
    for (u8 c: word) {
        hash = hash * 1099511628211ull ^ c;
    }
    s64 slot_i = hash % prep->cache_lookup.size;
    while (true) {
        auto slot = prep->cache_lookup[slot_i];
        if (slot.index == -1) break;
        if (slot.hash == hash) {
            *box = prep->cache[slot.index];
            return;
        }
        slot_i = (slot_i + 1) % prep->cache_lookup.size;
    }
    // Note that slot_i now points at the next empty slot

    Array_dyn<int> glyphs = prep->glyph_buf;
    defer { prep->glyph_buf = glyphs; };
    glyphs.size = 0;
    
    float f = context->fonts[font].scale;
    stbtt_fontinfo* fontinfo = &context->font_info[context->fonts[font].info_index];
    
    for (s64 i = 0; i < word.size;) {
        // Decode utf-8
        u32 c;
        s64 c_bytes = helper_decode_utf8(array_subarray(word, i, word.size), &c);
        
        s32 glyph = stbtt_FindGlyphIndex(fontinfo, c);
        if (glyph) array_push_back(&glyphs, glyph);
        
        i += c_bytes;
    }

    s64 x = prep->x;
    s64 y = prep->y;
    s64 y_incr = prep->y_incr;
    
    Array_dyn<u8> buf = prep->image_buf;
    defer { prep->image_buf = buf; };

    float shift = 0.f;
    bool draw = false;
    s64 x_orig = x;
    s64 y_orig = y;
    s64 x_init_off = 0;
    s64 y_incr_new = 0;
    s64 size = prep->size;
    
    for (s64 k = 0; k < glyphs.size; ++k) {
        int ix0, iy0, ix1, iy1;
        stbtt_GetGlyphBitmapBoxSubpixel(fontinfo, glyphs[k], f, f, shift, 0.f, &ix0, &iy0, &ix1, &iy1);

        s64 w = ix1 - ix0;
        s64 h = iy1 - iy0;

        int adv, lsb;
        stbtt_GetGlyphHMetrics(fontinfo, glyphs[k], &adv, &lsb);

        s64 x0 = x + ix0;
        s64 y0 = y + iy0;

        if (not draw) {
            if (x0 < x_orig+1) {
                x_init_off += x_orig+1 - x0;
                x += x_orig+1 - x0;
                x0 = x_orig+1;
            }
            if (y0 < y_orig+1) {
                s64 diff = y_orig+1 - y0;
                y += diff;
                y_incr_new += diff;
                y0 += diff;
            }
            if (y_incr_new < y0+h - y_orig) y_incr_new = y0+h - y_orig;
        } else {
            array_resize(&buf, w * h);
            stbtt_MakeGlyphBitmapSubpixel(fontinfo, buf.data, w, h, w, f, f, shift, 0.f, glyphs[k]);

            if ((x0 + w) + (y0 + h) * size > prep->image.size) {
                fprintf(stderr, "Error: glyph texture capacity exceeded\n");
                exit(202);
            }
            
            for (s64 row = 0; row < h; ++row) {
                for (s64 col = 0; col < w; ++col) {
                    u8* p = &prep->image[(x0 + col) + (y0 + row) * size];
                    *p = (u8)std::min(255, *p + buf[col + row*w]);
                }
            }
        }

        if (k+1 < glyphs.size) {
            int kern = stbtt_GetGlyphKernAdvance(fontinfo, glyphs[k], glyphs[k+1]);
            shift += (adv + kern/2) * letter_fac * f; // Should be just kern, but looks weird...
            float shift_f = std::floor(shift);
            if (shift - shift_f > 0.99) {shift_f += 1; shift += 0.01;}
            x += (s64)shift_f;
            shift -= shift_f;
        } else if (not draw) {
            // Execute the loop another time, but now we draw the glyphs
            draw = true;
            k = -1;
            shift = 0.f;
            box->x0 = (float)(1-x_init_off);
            box->y0 = (float)(y_orig+1 - y);
            box->x1 = (float)(x0 + w - x_orig - x_init_off);
            box->y1 = (float)(y_incr_new - y + y_orig+1);
            box->advance = (float)(x - x_orig - x_init_off) + adv*f;
            box->font = font;
            
            if (x0 + w < size) {
                x = x_orig + x_init_off;
                if (y_incr < y_incr_new) y_incr = y_incr_new;
            } else {
                x = x_init_off;
                y += y_incr;
                y_orig += y_incr;
                y_incr = y_incr_new;
            }

            box->s0 = (float)(box->x0 + x) / (float)size;
            box->t0 = (float)(box->y0 + y) / (float)size;
            box->s1 = (float)(box->x1 + x) / (float)size;
            box->t1 = (float)(box->y1 + y) / (float)size;
        } else {
            // Move to the next box
            x = x0 + w;
            y = y_orig;
        }
    }
    
    prep->x = x;
    prep->y = y;
    prep->y_incr = y_incr;
    prep->dirty = true;

    // Insert element into hashtable
    if (prep->cache.size*4 > prep->cache_lookup.size*3) {
        fprintf(stderr, "Error: Text_box cache size limit exceeded.\n");
        exit(201);
    }
    prep->cache_lookup[slot_i] = {hash, prep->cache.size};
    array_push_back(&prep->cache, *box);
}

void platform_text_prepare(int font_size, float small_frac, Array_t<Text_box>* offsets, float* linoff, float* ascent) {
    Lui_context* context = &global_platform.gl_context;
    
    _platform_init_font(Lui_context::FONT_BDD_NORMAL,  -1, font_size             );
    _platform_init_font(Lui_context::FONT_BDD_ITALICS, -1, font_size             );
    _platform_init_font(Lui_context::FONT_BDD_SMALL,   -1, font_size * small_frac);
    lui_text_prepare_init(&context->prep_bdd, 512);

    for (s64 i = 0; i < offsets->size; ++i) {
        u8 c = webgl_bddlabel_index_char(i);
        bool italicized;
        auto arr = webgl_bddlabel_index_utf8(i, nullptr, &italicized);

        u8 font = c & 128 ? Lui_context::FONT_BDD_SMALL :
            italicized ? Lui_context::FONT_BDD_ITALICS :
            Lui_context::FONT_BDD_NORMAL;
        
        Text_box box;
        lui_text_prepare_word(context, &context->prep_bdd, font, arr, &box);

        (*offsets)[i] = box;
    }
    
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, context->prep_bdd.size, context->prep_bdd.size, 0,
        GL_RED, GL_UNSIGNED_BYTE, context->prep_bdd.image.data);

    if (ascent) *ascent = context->fonts[Lui_context::FONT_BDD_NORMAL].ascent;
    if (linoff) *linoff = 0.f;
}

void lui_text_draw(Lui_context* context, Array_t<Text_box> boxes, s64 x_, s64 y_, s64 w_, u8* fill, s64* x_out, s64* y_out, bool only_measure=false, s64* xw_out=nullptr) {
    assert(context);

    if (not boxes.size) return;
    
    float x = (float)x_, y = (float)y_, w = (float)w_;
    float orig_x = x, orig_y = y, max_x = x;
    if (w_ == -1) w = INFINITY;

    u8 black[] = {  0,  0,   0, 255};
    u8 red[]   = {112, 10,  19, 255};
    if (not fill) fill = black;

    y = std::round(y + context->fonts[boxes[0].font].ascent);

    for (s64 i = 0; i < boxes.size; ++i) {
        Text_box box = boxes[i];
        auto font_inst = context->fonts[box.font];

        float word_end = x;
        for (s64 j = i; j < boxes.size; ++j) {
            if ((boxes[j].flags & Text_fmt::STICKY) and not (boxes[j].flags & Text_fmt::GROUP_BREAKING)) {
                float adv = boxes[j].advance;
                if (~box.flags & Text_fmt::NOSPACE) adv += context->fonts[boxes[j].font].space;
                word_end += std::round(adv);
            } else {
                word_end += boxes[j].x1;
                break;
            }
        }
        if (x > orig_x and word_end > orig_x + w) {
            x = orig_x;
            y = std::round(y + font_inst.newline);
            if (box.flags & (Text_fmt::INDENTED | Text_fmt::ITEMIZED)) {
                x += std::round(font_inst.space * 4);
            }
        } else if (x == orig_x and (box.flags & Text_fmt::ITEMIZED)) {
            x += std::round(font_inst.space * 3 - box.advance);
        }
        max_x = std::max(x + box.x1, max_x);

        u8* box_fill = box.flags & Text_fmt::RED ? red : fill;

        if (not only_measure) {
            array_append(&context->buf_uitext_pos, {
                x+box.x0, y+box.y0, x+box.x1, y+box.y0, x+box.x1, y+box.y1,
                x+box.x0, y+box.y0, x+box.x1, y+box.y1, x+box.x0, y+box.y1
            });
            array_append(&context->buf_uitext_tpos, {
                box.s0, box.t0, box.s1, box.t0, box.s1, box.t1, box.s0, box.t0, box.s1, box.t1, box.s0, box.t1
            });
            
            for (s64 j = 0; j < 6; ++j) {
                array_append(&context->buf_uitext_fill, {box_fill, 4});
            }
        }
        
        if (box.flags & Text_fmt::NOSPACE) {
            x = std::round(x + box.advance);
        } else {
            x = std::round(x + box.advance + font_inst.space);
        }
        
        if (box.flags & Text_fmt::PARAGRAPH) {
            x = orig_x;
            y = std::round(y + font_inst.newline * 1.5f);
        } else if (box.flags & Text_fmt::PARAGRAPH_CLOSE) {
            x = orig_x;
            y = std::round(y + font_inst.newline * 1.2f);
        } else if (box.flags & Text_fmt::NEWLINE) {
            x = orig_x;
            y = std::round(y + font_inst.newline);
        }
    }

    auto font_inst = context->fonts[boxes[boxes.size-1].font];
    y -= font_inst.ascent;
    
    if (x_out)  *x_out  = (s64)std::round(x);
    if (y_out)  *y_out  = (s64)std::round(y);
    if (xw_out) *xw_out = (s64)std::ceil(max_x);
}

void lui_draw_buttonlike(Lui_context* context, Rect bb, Padding pad, u8 flags, Rect* text_bb, bool only_measure=false) {
    assert(context);

    if (text_bb) {
        text_bb->x = bb.x + pad.mar_x + pad.pad_x;
        text_bb->y = bb.y + pad.mar_y + pad.pad_y;
        text_bb->w = bb.w - 2*pad.mar_x - 2*pad.pad_x;
        text_bb->h = bb.h - 2*pad.mar_y - 2*pad.pad_y;
    }
    if (only_measure) return;
    
    // Due to the way the shader is defined we have to offset everything by half a pixel.

    float pad_x = (float)pad.pad_x;
    float pad_y = (float)pad.pad_y;
    float mar_x = (float)pad.mar_x;
    float mar_y = (float)pad.mar_y;

    float w = (float)bb.w - 2.f*mar_x;
    float h = (float)bb.h - 2.f*mar_y;

    float f = std::min(std::min(w, h), (float)context->width_button_max);
    bool split_x = f < w;
    bool split_y = f < h;
    
    float x1 = (float)bb.x;
    float x4 = (float)bb.x + w + 2.f*mar_x;
    float x2 = x1 + mar_x + f/2.f;
    float x3 = x4 - mar_x - f/2.f;
    float y1 = (float)bb.y;
    float y4 = (float)bb.y + h + 2.f*mar_y;
    float y2 = y1 + mar_y + f/2.f;
    float y3 = y4 - mar_y - f/2.f;

    float sx = f * 0.5f + mar_x;
    float sy = f * 0.5f + mar_y;

    s64 vertices;
    if (split_x and not split_y) {
        array_append(&context->buf_uibutton_pos, {
            x1, y4, x1, y4, x1, y1, x2, y4, x2, y1, x3, y4, x3, y1, x4, y4, x4, y1, x4, y1
        });
        array_append(&context->buf_uibutton_x, {
            -sx, sy, -sx, sy, -sx, -sy, 0.f, sy, 0.f, -sy, 0.f, sy, 0.f, -sy, sx, sy, sx, -sy, sx, -sy
        });
        vertices = 10;
    } else if (not split_x and split_y) {
        array_append(&context->buf_uibutton_pos, {
            x1, y1, x1, y1, x4, y1, x1, y2, x4, y2, x1, y3, x4, y3, x1, y4, x4, y4, x4, y4
        });
        array_append(&context->buf_uibutton_x, {
            -sx, -sy, -sx, -sy, sx, -sy, -sx, 0.f, sx, 0.f, -sx, 0.f, sx, 0.f, -sx, sy, sx, sy, sx, sy
        });
        vertices = 10;
    } else if (not split_x and not split_y) {
        array_append(&context->buf_uibutton_pos, {
            x1, y1, x1, y1, x4, y1, x1, y4, x4, y4, x4, y4
        });
        array_append(&context->buf_uibutton_x, {
            -sx, -sy, -sx, -sy, sx, -sy, -sx, sy, sx, sy, sx, sy
        });
        vertices = 6;
    } else  {
        array_append(&context->buf_uibutton_pos, {
            x1, y1, x1, y1, x2, y1, x1, y2, x2, y2, x1, y3, x2, y3, x1, y4, x2, y4, x3, y4, x2, y3,
            x3, y3, x2, y2, x3, y2, x2, y1, x3, y1, x4, y1, x3, y2, x4, y2, x3, y3, x4, y3, x3, y4,
            x4, y4, x4, y4
        });
        array_append(&context->buf_uibutton_x, {
            -sx, -sy, -sx, -sy, 0.f, -sy, -sx, 0.f, 0.f, 0.f, -sx, 0.f, 0.f, 0.f, -sx, sy, 0.f, sy, 0.f, sy, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, -sy, 0.f, -sy, sx, -sy, 0.f, 0.f, sx, 0.f, 0.f, 0.f, sx, 0.f, 0.f, sy, sx, sy, sx, sy
        });
        vertices = 24;
    }

    float grad_light_t = std::min(1.f, 15.f / (float)bb.h);
    
    float border_normal[] = { 1.1, 0.72, 0.55 };
    float border_active[] = { 1.25, 0.67, 0.5 };
    float border_thick [] = { 1.5, 0.62, 0.45 };
    float border_entry [] = { 1.0, 0.55, 0.72 };
    float border_gray  [] = { 0.97, 0.78, 0.78 };
    float grad_normal  [] = { 0.85, 1.0, 0.92, 0.95 };
    float grad_pressed [] = { 0.15, 0.9, 0.87, 0.95 };
    float grad_light   [] = { grad_light_t, 0.95, 1.0, 1.0 };
    float grad_flat    [] = { 1.0, 0.95, 0.95, 0.95 };
    float color_normal [] = { 1.0, 1.0, 1.0 };
    float color_focused[] = { 1.32, 0.83, 0.28 };
    
    bool inwards = flags & (Lui_context::DRAW_ENTRY | Lui_context::DRAW_RADIO);
    float* border = flags & Lui_context::DRAW_DISABLED ? border_gray :
        flags & Lui_context::DRAW_ENTRY ? border_entry :
        flags & Lui_context::DRAW_PRESSED ? border_thick :
        flags & Lui_context::DRAW_ACTIVE ? border_active :
        flags & Lui_context::DRAW_RADIO ? border_entry
        : border_normal;
    float* color = flags & Lui_context::DRAW_FOCUSED ? color_focused : color_normal;
    float* grad = flags & Lui_context::DRAW_DISABLED ? grad_flat :
        inwards ? grad_light :
        flags & Lui_context::DRAW_PRESSED ? grad_pressed
        : grad_normal;
    float size = inwards ? f * 0.5f - 0.15f : f * 0.5f;
    float inner = ~flags & Lui_context::DRAW_RADIO ? 1.f :
        ~flags & Lui_context::DRAW_PRESSED ? 1.f :
        flags & Lui_context::DRAW_DISABLED ? 0.7f
        : 0.2f;
    float circle = flags & Lui_context::DRAW_RADIO ? 2.f : 8.f;
    
    for (s64 i = 0; i < vertices; ++i) {
        array_push_back(&context->buf_uibutton_size, size);
        array_append(&context->buf_uibutton_border, {border, 3});
        array_append(&context->buf_uibutton_color, {color, 3});
        array_append(&context->buf_uibutton_grad, {grad, 4});
        array_append(&context->buf_uibutton_circle, {circle, inner});
    }
}

void lui_draw_entry_text(Lui_context* context, Text_entry entry, Rect text_bb, u8 font, bool cursor) {
    auto font_inst = context->fonts[font];
    
    float tx = (float)text_bb.x, ty = (float)text_bb.y, tw = (float)text_bb.w, th = (float)text_bb.h;
    float y = std::round(ty + font_inst.ascent);

    s64 c_i = 0;

    float newline = std::round(font_inst.newline);
    
    s64 offset_rows = (s64)std::floor(entry.offset_y / newline);
    for (s64 row = 0; row < offset_rows; ++row) {
        while (c_i < entry.text.size and entry.text[c_i++] != '\n');
    }
    y -= std::round((float)entry.offset_y - (float)offset_rows * newline);

    s64 sel0 = -1, sel1 = -1;
    if (entry.selection != -1) {
        sel0 = std::min(entry.selection, entry.cursor);
        sel1 = std::max(entry.selection, entry.cursor);
    }

    u8 gray1[] = { 20,  20,  20, 255};
    u8 gray2[] = {140, 140, 140, 255};
    u8 sel_f[] = {255, 255, 255, 255};
    u8 sel_1[] = {140, 140, 140, 255};
    u8 sel_2[] = {242, 152,  51, 255};
    u8* normal = context->elem_flags[entry.slot] & Lui_context::DRAW_DISABLED ? gray2 : gray1;
    u8* sel_b = context->elem_flags[entry.slot] & Lui_context::DRAW_FOCUSED ? sel_2 : sel_1;
    
    while (true) {
        if (y - font_inst.ascent >= ty + th) break;

        float x = tx - (float)entry.offset_x;
        while (true) {
            u8* fill = normal;
            if (sel0 <= c_i and c_i < sel1) {
                fill = sel_f;
            }
            if (entry.selection == -1 and cursor and c_i == entry.cursor) {
                float p[] = {x-1, y-font_inst.ascent+font_inst.height*0.1f, 1, font_inst.height*0.8f};
                bool draw = true;
                if (p[0] + p[2] <= tx-1) draw = false;
                if (p[1] + p[3] <= ty-1) draw = false;
                if (p[0] >= tx+tw+1) draw = false;
                if (p[1] >= ty+th+1) draw = false;
                if (p[0] < tx-1) p[0] = tx-1;
                if (p[0]+p[2] > tx+tw+1) p[2] = tx+tw+1 - p[0];
                if (p[1] < ty-1) p[1] = ty-1;
                if (p[1]+p[3] > ty+th+1) p[3] = ty+th+1 - p[1];
                if (draw) lui_draw_rect(context, p[0], p[1], p[2], p[3], Lui_context::LAYER_FRONT, fill);
            }
            
            if (c_i >= entry.text.size) break;
            // Not entirely accurate, due to left-side-bearing, but should not matter
            if (x >= tx + tw) {
                while (c_i < entry.text.size and entry.text[c_i] != '\n') c_i++;
                break;
            }

            s64 c_len = helper_decode_utf8(array_subarray(entry.text, c_i, entry.text.size));

            Text_box box;
            lui_text_prepare_word(context, &context->prep_ui, font, array_subarray(entry.text, c_i, c_i+c_len), &box);
            box.x0 += x; box.x1 += x; box.y0 += y; box.y1 += y;

            if (sel0 <= c_i and c_i < sel1) {
                float y0 = y - font_inst.ascent - 0.5*(newline - font_inst.ascent);
                float rx0 = std::max(tx, x);
                float ry0 = std::max(ty, y0);
                float rx1 = std::min(tx + tw, x + (entry.text[c_i] == '\n' ? font_inst.space : box.advance));
                float ry1 = std::min(ty + th, y0 + newline);
                if (rx0 < rx1 and ry0 < ry1) {
                    lui_draw_rect(context, rx0, ry0, rx1 - rx0, ry1 - ry0, Lui_context::LAYER_MIDDLE, sel_b);
                }
            }
            
            if (entry.text[c_i] == '\n') break;
            
            x = std::round(x + box.advance);
            c_i += c_len;

            if (box.x1 <= tx     ) continue;
            if (box.x0 >= tx + tw) continue;
            if (box.y1 <= ty     ) continue;
            if (box.y0 >= ty + th) continue;
            
            if (box.x0 < tx and tx < box.x1) {
                box.s0 += (tx - box.x0) / (box.x1 - box.x0) * (box.s1 - box.s0);
                box.x0 = tx;
            }
            if (box.x0 < tx+tw and tx+tw < box.x1) {
                box.s1 += (tx+tw - box.x1) / (box.x1 - box.x0) * (box.s1 - box.s0);
                box.x1 = tx+tw;
            }
            if (box.y0 < ty and ty < box.y1) {
                box.t0 += (ty - box.y0) / (box.y1 - box.y0) * (box.t1 - box.t0);
                box.y0 = ty;
            }
            if (box.y0 < ty+th and ty+th < box.y1) {
                box.t1 += (ty+th - box.y1) / (box.y1 - box.y0) * (box.t1 - box.t0);
                box.y1 = ty+th;
            }

            array_append(&context->buf_uitext_pos, {
                box.x0, box.y0, box.x1, box.y0, box.x1, box.y1,
                box.x0, box.y0, box.x1, box.y1, box.x0, box.y1
            });
            array_append(&context->buf_uitext_tpos, {
                box.s0, box.t0, box.s1, box.t0, box.s1, box.t1,
                box.s0, box.t0, box.s1, box.t1, box.s0, box.t1
            });
            for (s64 j = 0; j < 6; ++j) {
                array_append(&context->buf_uitext_fill, {fill, 4});
            }
        }
        if (c_i >= entry.text.size) break;

        y += newline;
        ++c_i;
    }
}

void platform_fmt_init() {
    global_platform.gl_context.fmt_flags = 0;
}
void platform_fmt_begin(u64 flags) {
    global_platform.gl_context.fmt_flags |= flags;
}
void platform_fmt_end(u64 flags) {
    Lui_context* context = &global_platform.gl_context;
    if (context->fmt_boxes.size) {
        context->fmt_boxes[context->fmt_boxes.size-1].flags |= flags & Text_fmt::GROUP_SPACING;
    }
    
    context->fmt_flags &= ~flags;
}
void platform_fmt_text(u64 flags_add, Array_t<u8> text) {
    Lui_context* context = &global_platform.gl_context;
    
    platform_fmt_begin(flags_add);
    u64 flags = global_platform.gl_context.fmt_flags;

    u8 font;
    if (flags & Text_fmt::HEADER) {
        font = Lui_context::FONT_LUI_HEADER;
    } else if (flags & Text_fmt::BOLD) {
        font = Lui_context::FONT_LUI_BOLD;
    } else if (flags & Text_fmt::ITALICS) {
        font = Lui_context::FONT_LUI_ITALIC;
    } else if (flags & Text_fmt::SMALL) {
        font = Lui_context::FONT_LUI_SMALL;
    } else if (flags & Text_fmt::SANS) {
        font = Lui_context::FONT_LUI_SANS;
    } else if (flags & Text_fmt::BUTTON) {
        font = Lui_context::FONT_LUI_BUTTON;
    } else {
        font = Lui_context::FONT_LUI_NORMAL;
    }

    float letter_fac = flags & Text_fmt::COMPACT ? 0.95f : 1.f;
    
    s64 last = 0;
    for (s64 i = 0; i <= text.size; ++i) {
        // Digits are liable to change, so we render them individually
        bool isdigit = (last < text.size and ('0' <= text[last] and text[last] <= '9'))
                    or (i    < text.size and ('0' <= text[i]    and text[i]    <= '9'));
        if (i < text.size and text[i] != ' ' and text[i] != '\n' and not isdigit) continue;
        
        if (last == i) continue;

        Text_box box;
        lui_text_prepare_word(&global_platform.gl_context, &context->prep_ui, font, array_subarray(text, last, i), &box, letter_fac);
        if (isdigit) box.flags |= Text_fmt::NOSPACE | Text_fmt::STICKY;

        box.flags |= flags & Text_fmt::GROUP_DRAWING;
        
        array_push_back(&global_platform.gl_context.fmt_boxes, box);
        last = i + not isdigit;
    }
    
    platform_fmt_end(flags_add);
}
void platform_fmt_spacing(u64 flags) {
    Text_box box;
    box.font = Lui_context::FONT_LUI_NORMAL;
    box.flags = flags;
    array_push_back(&global_platform.gl_context.fmt_boxes, box);
}

void platform_fmt_store(s64 slot) {
    assert(0 <= slot);

    Lui_context* context = &global_platform.gl_context;
    
    context->fmt_slots[slot].size = 0;
    array_append(&context->fmt_slots[slot], context->fmt_boxes);
    context->fmt_boxes.size = 0;
}
void platform_fmt_draw(s64 slot, s64 x, s64 y, s64 w, s64* x_out=nullptr, s64* y_out=nullptr, bool only_measure=false, s64* xw_out=nullptr) {
    Lui_context* context = &global_platform.gl_context;
    u8 black[] = {0, 0, 0, 255};
    u8 gray[] = {120, 120, 120, 255};
    u8* fill = context->elem_flags[slot] & Lui_context::DRAW_DISABLED ? gray : black;
    
    s64 xw, yh;
    lui_text_draw(context, context->fmt_slots[slot], x, y, w, fill, x_out, &yh, only_measure, &xw);
    
    context->elem_bb[slot] = {x, y, xw, yh-y};
    
    if (xw_out) *xw_out = xw;
    if (y_out) *y_out = yh;
}
void platform_fmt_store_copy(s64 slot_into, s64 slot_from) {
    Lui_context* context = &global_platform.gl_context;
    context->fmt_slots[slot_into].size = 0;
    array_append(&context->fmt_slots[slot_into], context->fmt_slots[slot_from]);
}

void lui_draw_button_right(Lui_context* context, s64 slot, s64 x, s64 y, s64 w, s64* ha_out, s64* w_out) {
    assert(context);

    s64 text_w = 0, text_h = 0;
    u8 font = Lui_context::FONT_LUI_BUTTON;
    
    Array_t<Text_box> boxes = context->fmt_slots[slot];
    if (boxes.size) {
        font = boxes[0].font;
        lui_text_draw(context, boxes, 0, 0, -1, nullptr, &text_w, nullptr, true);
        text_h = (s64)std::round(context->fonts[font].height);
    }
    
    context->elem_flags[slot] |= Lui_context::DRAW_BUTTON;
    u64 flags = context->elem_flags[slot];

    Padding pad {12, 4, 3, 3};
    if (flags & Lui_context::DRAW_COMPACT) pad.pad_x = 9;
    
    Rect bb;
    bb.w = 2*pad.mar_x + 2*pad.pad_x + text_w;
    bb.h = 2*pad.mar_y + 2*pad.pad_y + text_h;
    bb.x = x+w - bb.w;
    bb.y = y;
    context->elem_bb[slot] = bb;
    
    Rect text_bb;
    lui_draw_buttonlike(context, bb, pad, flags, &text_bb);

    s64 xoff = flags & Lui_context::DRAW_PRESSED ? 1 : 0;
    u8 gray1[] = { 70,  70,  70, 255};
    u8 gray2[] = {140, 140, 140, 255};
    u8* fill = flags & Lui_context::DRAW_DISABLED ? gray2 : gray1;
    lui_text_draw(context, boxes, text_bb.x + xoff, text_bb.y, -1, fill, &text_bb.x, &text_bb.y);

    if (ha_out) *ha_out = text_bb.y - y + context->fonts[font].ascent - context->fonts[Lui_context::FONT_LUI_NORMAL].ascent;
    if (w_out) *w_out = w - bb.w;
}

void lui_draw_entry(Lui_context* context, Text_entry* entry, s64 x, s64 y, s64 w, s64 rows, s64* x_out, s64* y_out, s64* ha_out, Resizer* resizer=nullptr, bool only_measure=false) {
    u8 font = Lui_context::FONT_LUI_BUTTON;
    auto font_inst = context->fonts[font];
    Padding pad = {7, 5, 3, 3};
    context->padding_entry = pad;

    s64 h = resizer ? resizer->size : rows * (s64)std::round(font_inst.newline);
    
    Rect bb {x, y, w, -1};
    bb.h = h + pad.mar_y*2 + pad.pad_y*2;
    context->elem_bb[entry->slot] = bb;

    context->elem_flags[entry->slot] |= Lui_context::DRAW_ENTRY;
    u64 flags = context->elem_flags[entry->slot];
    
    Rect text_bb;
    lui_draw_buttonlike(context, bb, pad, flags, &text_bb, only_measure);

    entry->draw_w = text_bb.w;
    entry->draw_h = text_bb.h;

    if (not only_measure) {
        lui_draw_entry_text(context, *entry, text_bb, font, entry->cursor_draw);
    }

    if (resizer) {
        s64 rh = context->width_resizer;
        context->elem_bb[resizer->slot] = Rect {bb.x, bb.y+bb.h - rh/2, bb.w, (rh+1)/2};
        context->elem_flags[resizer->slot] |= Lui_context::DRAW_RESIZER;
    }
    
    if (x_out) *x_out = bb.x + bb.w;
    if (y_out) *y_out = bb.y + bb.h;
    if (ha_out) *ha_out = text_bb.y - y + font_inst.ascent - context->fonts[Lui_context::FONT_LUI_NORMAL].ascent;
}

void lui_draw_radio(Lui_context* context, s64 x, s64 y, s64 slot, s64* x_out, s64* y_out) {
    auto font_inst = context->fonts[Lui_context::FONT_LUI_NORMAL];

    context->elem_flags[slot] |= Lui_context::DRAW_RADIO;
    u64 flags = context->elem_flags[slot];

    s64 size = 21;
    Padding pad {3, 3, 5, 5};
    Rect bb {x, y + (s64)std::round(font_inst.ascent) - size + pad.mar_y, size, size};
    lui_draw_buttonlike(context, bb, pad, flags, nullptr);

    s64 x1, y1;
    platform_fmt_draw(slot, x + size, y, -1, &x1, &y1);

    context->elem_bb[slot] = {x, y, x1 - x, (s64)std::round(font_inst.height)};
    
    if (x_out) *x_out = x1;
    if (y_out) *y_out = y1;
}

void lui_draw_scrollbar(Lui_context* context, Scrollbar* scroll) {
    Padding pad {0, 0};
    u8 orange[] = {242, 152,  51, 255};
    u8 black[] = {200, 200, 200, 255};
    u8* fill = context->elem_flags[scroll->slot] & Lui_context::DRAW_ACTIVE ? orange : black;

    context->elem_flags[scroll->slot] |= Lui_context::DRAW_SCROLL;
    
    auto bb = context->elem_bb[scroll->slot_area];
    if (bb.h >= scroll->total_height) {
        context->elem_bb[scroll->slot] = Rect {-10, -10, 0, 0};
    } else {
        s64 x = bb.x + bb.w - context->width_scrollbar - pad.pad_x;
        s64 y = bb.y + pad.pad_y;
        s64 h = bb.h - pad.pad_y * 2;
        s64 p_offset = h * scroll->offset / scroll->total_height;
        s64 p_height = h * bb.h / scroll->total_height;
        Rect r {x, y + p_offset, context->width_scrollbar, p_height};
        context->elem_bb[scroll->slot] = r;
        if (~context->elem_flags[scroll->slot] & Lui_context::DRAW_ACTIVE) {
            r.x += r.w / 2;
            r.w = (r.w + 1) / 2;
        }
        lui_draw_rect(context, r.x, r.y, r.w, r.h, Lui_context::LAYER_MIDDLE, fill);
    }
}

void _platform_frame_init() {
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
    auto context = &global_platform.gl_context;
    
    context->buf_uirect_pos.size = 0;
    context->buf_uirect_fill.size = 0;

    context->buf_uitext_pos.size = 0;
    context->buf_uitext_tpos.size = 0;
    context->buf_uitext_fill.size = 0;
    
    context->buf_uibutton_pos.size = 0;
    context->buf_uibutton_x.size = 0;
    context->buf_uibutton_size.size = 0;
    context->buf_uibutton_border.size = 0;
    context->buf_uibutton_color.size = 0;
    context->buf_uibutton_grad.size = 0;
    context->buf_uibutton_circle.size = 0;
}

void _platform_init(Platform_state* platform) {
    assert(platform);
    Lui_context* context = &global_platform.gl_context; // The macros expect a local named context
    
    _platform_init_gl(platform);

    // Font stuff
    context->font_info = array_create<stbtt_fontinfo>(context->font_files.size);

    // Note: Ignore the last one, as that is the license
    for (s64 i = 0; i+1 < context->font_info.size; ++i) {
        int code = stbtt_InitFont(&context->font_info[i], context->font_file_data[i].data, 0);
        if (code == 0) {
            fprintf(stderr, "Error: Could not parse font data in file %s\n", context->font_files[i]);
            exit(101);
        }
    }

    context->fonts = array_create<Lui_context::Font_instance>(Lui_context::FONT_COUNT);

    // Note that the indices at the second to last parameter refer to the indices of the font files
    // in Lui_context::font_files
    _platform_init_font(Lui_context::FONT_LUI_NORMAL, 0, 20);
    _platform_init_font(Lui_context::FONT_LUI_ITALIC, 1, 20);
    _platform_init_font(Lui_context::FONT_LUI_BOLD, 2, 20);
    _platform_init_font(Lui_context::FONT_LUI_HEADER, 2, 26);
    _platform_init_font(Lui_context::FONT_LUI_SMALL, 0, 15);
    _platform_init_font(Lui_context::FONT_LUI_SANS, 3, 20);
    _platform_init_font(Lui_context::FONT_LUI_BUTTON, 3, 16.7);
    _platform_init_font(Lui_context::FONT_BDD_NORMAL, 3, 20);
    _platform_init_font(Lui_context::FONT_BDD_ITALICS, 1, 20);
    _platform_init_font(Lui_context::FONT_BDD_SMALL, 3, 20);

    // Initialise font preparation
    lui_text_prepare_init(&context->prep_ui, 1024);
    // prep_bdd is initialised in platform_text_prepare

    // Initialise formatted font rendering
    array_resize(&context->fmt_slots, Lui_context::SLOT_COUNT);
    
    // Populate all the fixed slots
    platform_fmt_init();
    platform_fmt_text(Text_fmt::PARAGRAPH | Text_fmt::HEADER, "Binary Decision Diagrams");
    platform_fmt_text(Text_fmt::PARAGRAPH, "This is obst, a visualisation of algorithms related to Binary Decision Diagrams, written by Philipp Czerner in 2018.");
    platform_fmt_text(Text_fmt::PARAGRAPH, u8"Read the help for more information, or get started right away by pressing “Create and Add”."); //@Cleanup: Remove the utf-8 string, replace by raw bytes
    platform_fmt_text(Text_fmt::ITALICS, "Hint:");
    platform_fmt_text(Text_fmt::PARAGRAPH, "You can hover over nodes using your cursor, showing additional details.");
    platform_fmt_store(Lui_context::SLOT_INITTEXT);

    platform_fmt_init();
    platform_fmt_text(Text_fmt::PARAGRAPH | Text_fmt::HEADER, "About");
    platform_fmt_text(Text_fmt::PARAGRAPH, "obst visualises algorithms related to Binary Decision Diagrams (BDDs). BDDs are a data structure one can use to represent sets of numbers in a concise and unique fashion, while still being able to efficiently execute set operations on them.");
    platform_fmt_text(Text_fmt::PARAGRAPH, "In particular, this tool shows how to create a BDD from a list of numbers, as well as compute their union, intersection and complement. The source is publicly available on GitHub. Feel free to write me an email for any feedback, suggestions, etc.!");
    platform_fmt_spacing(Text_fmt::NEWLINE);
    platform_fmt_text(Text_fmt::PARAGRAPH | Text_fmt::HEADER, "Usage Instructions");
    platform_fmt_text(Text_fmt::PARAGRAPH, u8"Press “Create and add” to get started. This will add a BDD to the graph representing the set of numbers you specified. Afterwards you can step through the frames of the animation (using arrow keys, or the buttons labelled “◁” and “▷”). ");
    platform_fmt_text(Text_fmt::PARAGRAPH, "Once the graph contains some nodes, you can start doing set operations. Simply enter the names of the nodes and press the “Calculate” button. Once again, you can go through the frames of the animation to find out how exactly the result was computed.");
    platform_fmt_spacing(Text_fmt::NEWLINE);
    platform_fmt_text(Text_fmt::PARAGRAPH | Text_fmt::HEADER, "Keybindings");
    platform_fmt_begin(Text_fmt::ITEMIZED);
    platform_fmt_text(0, u8"•");
    platform_fmt_text(Text_fmt::ITALICS | Text_fmt::NOSPACE, "Left, Right");
    platform_fmt_text(Text_fmt::NEWLINE, ": Move to the previous/next frame of the animation.");
    platform_fmt_text(0, u8"•");
    platform_fmt_text(Text_fmt::ITALICS | Text_fmt::NOSPACE, "Page down, Page up");
    platform_fmt_text(Text_fmt::NEWLINE, ": Move to the previous/next checkpoint, that is the first frame of an operation.");
    platform_fmt_text(0, u8"•");
    platform_fmt_text(Text_fmt::ITALICS | Text_fmt::NOSPACE, "Home, End");
    platform_fmt_text(Text_fmt::NEWLINE, ": Move to the first/last frame.");
    platform_fmt_text(0, u8"•");
    platform_fmt_text(Text_fmt::ITALICS | Text_fmt::NOSPACE, "F1");
    platform_fmt_text(Text_fmt::PARAGRAPH, ": Show/hide help.");
    platform_fmt_end(Text_fmt::ITEMIZED);
    platform_fmt_text(Text_fmt::PARAGRAPH | Text_fmt::HEADER, "Font license");
    platform_fmt_text(Text_fmt::PARAGRAPH, "Fonts of the DejaVu family are used by obst. Their license information is included either in the fonts subdirectory, or can be viewed by running obst with --font-license as argument.");
    platform_fmt_store(Lui_context::SLOT_HELPTEXT);

    platform_fmt_store_simple(Text_fmt::PARAGRAPH, "Adds the BDD to the graph.", Lui_context::SLOT_BUTTON_DESC_CREATE);
    platform_fmt_store_simple(Text_fmt::PARAGRAPH, "Applies the operation.", Lui_context::SLOT_BUTTON_DESC_OP);
    platform_fmt_store_simple(Text_fmt::PARAGRAPH, "Reset the application, delete all nodes.", Lui_context::SLOT_BUTTON_DESC_REMOVEALL);
    platform_fmt_store_simple(Text_fmt::PARAGRAPH, "Display usage instructions.", Lui_context::SLOT_BUTTON_DESC_HELP);

    platform_fmt_store_simple(Text_fmt::COMPACT, "Input:", Lui_context::SLOT_LABEL_INPUT);
    platform_fmt_store_simple(0, "List of numbers", Lui_context::SLOT_LABEL_NUMBERS);
    platform_fmt_store_simple(0, "Boolean formula", Lui_context::SLOT_LABEL_FORMULA);
    platform_fmt_store_simple(0, "Base:", Lui_context::SLOT_LABEL_BASE);
    platform_fmt_store_simple(0, "Bit order:", Lui_context::SLOT_LABEL_BITORDER);
    platform_fmt_store_simple(0, "Variable order:", Lui_context::SLOT_LABEL_VARORDER);
    platform_fmt_store_simple(Text_fmt::COMPACT, "Operation:", Lui_context::SLOT_LABEL_OPERATION);
    platform_fmt_store_simple(Text_fmt::COMPACT, "Union", Lui_context::SLOT_LABEL_UNION);
    platform_fmt_store_simple(Text_fmt::COMPACT, "Intersection", Lui_context::SLOT_LABEL_INTERSECTION);
    platform_fmt_store_simple(Text_fmt::COMPACT, "Complement", Lui_context::SLOT_LABEL_COMPLEMENT);
    platform_fmt_store_simple(0, "First node:", Lui_context::SLOT_LABEL_FIRSTNODE);
    platform_fmt_store_simple(0, "Second node:", Lui_context::SLOT_LABEL_SECONDNODE);

    u64 button_flag = Text_fmt::BUTTON | Text_fmt::COMPACT | Text_fmt::NOSPACE;
    platform_fmt_store_simple(button_flag, "Create and add", Lui_context::SLOT_BUTTON_CREATE);
    platform_fmt_store_simple(button_flag, "Calculate union", Lui_context::SLOT_LABEL_OP_U);
    platform_fmt_store_simple(button_flag, "Calculate intersection", Lui_context::SLOT_LABEL_OP_I);
    platform_fmt_store_simple(button_flag, "Calculate complement", Lui_context::SLOT_LABEL_OP_C);
    platform_fmt_store_simple(button_flag, "Remove all", Lui_context::SLOT_BUTTON_REMOVEALL);
    platform_fmt_store_simple(button_flag, "Show help", Lui_context::SLOT_LABEL_HELP_SHOW);
    platform_fmt_store_simple(button_flag, "Hide help", Lui_context::SLOT_LABEL_HELP_HIDE);
    platform_fmt_store_simple(button_flag, u8"◁", Lui_context::SLOT_BUTTON_PREV);
    platform_fmt_store_simple(button_flag, u8"▷", Lui_context::SLOT_BUTTON_NEXT);
    platform_fmt_store_simple(button_flag, u8"×", Lui_context::SLOT_BUTTON_HELP_CLOSE);

    platform_fmt_store_copy(Lui_context::SLOT_BUTTON_OP, Lui_context::SLOT_LABEL_OP_U);
    platform_fmt_store_copy(Lui_context::SLOT_BUTTON_HELP, Lui_context::SLOT_LABEL_HELP_SHOW);
    
    platform_fmt_init();
    platform_fmt_text(Text_fmt::BOLD, "Step-by-step");
    platform_fmt_text(Text_fmt::SMALL | Text_fmt::PARAGRAPH, "(Move using arrow keys)");
    platform_fmt_store(Lui_context::SLOT_BUTTON_DESC_CONTEXT);

    // Initialise elements
    context->elem_flags = array_create<u64> (Lui_context::SLOT_COUNT);
    context->elem_bb    = array_create<Rect>(Lui_context::SLOT_COUNT);

    context->entries = array_create<Text_entry>(Lui_context::ENTRY_COUNT);
    for (Text_entry& i: context->entries) i.selection = -1;
    context->entries[Lui_context::ENTRY_NUMBERS].slot    = Lui_context::SLOT_ENTRY_NUMBERS;
    context->entries[Lui_context::ENTRY_FORMULA].slot    = Lui_context::SLOT_ENTRY_FORMULA;
    context->entries[Lui_context::ENTRY_BASE].slot       = Lui_context::SLOT_ENTRY_BASE;
    context->entries[Lui_context::ENTRY_BITORDER].slot   = Lui_context::SLOT_ENTRY_BITORDER;
    context->entries[Lui_context::ENTRY_VARORDER].slot   = Lui_context::SLOT_ENTRY_VARORDER;
    context->entries[Lui_context::ENTRY_FIRSTNODE].slot  = Lui_context::SLOT_ENTRY_FIRSTNODE;
    context->entries[Lui_context::ENTRY_SECONDNODE].slot = Lui_context::SLOT_ENTRY_SECONDNODE;
    
    context->entries[Lui_context::ENTRY_BASE].disable_newline = true;
    context->entries[Lui_context::ENTRY_BITORDER].disable_newline = true;
    context->entries[Lui_context::ENTRY_VARORDER].disable_newline = true;
    context->entries[Lui_context::ENTRY_FIRSTNODE].disable_newline = true;
    context->entries[Lui_context::ENTRY_SECONDNODE].disable_newline = true;

    context->elem_flags[Lui_context::SLOT_BUTTON_NEXT] |= Lui_context::DRAW_COMPACT;
    context->elem_flags[Lui_context::SLOT_BUTTON_PREV] |= Lui_context::DRAW_COMPACT;

    context->elem_flags[Lui_context::SLOT_LABEL_NUMBERS] |= Lui_context::DRAW_PRESSED;

    array_printf(&context->entries[Lui_context::ENTRY_NUMBERS].text, "2, 4, 13, 17, 20, 24, 25, 31, 33, 41, 51, 52, 61, 62");
    array_printf(&context->entries[Lui_context::ENTRY_FORMULA].text, "f = x2 ^ x4\n(x1&f) <-> ~x3");
    
    array_printf(&context->entries[Lui_context::ENTRY_BASE].text, "10");
    array_printf(&context->entries[Lui_context::ENTRY_BITORDER].text, "auto");
    array_printf(&context->entries[Lui_context::ENTRY_VARORDER].text, "auto");

    context->scroll_panel.anim.duration = 0.1;
    context->scroll_panel.slot = Lui_context::SLOT_SCROLL_PANEL;
    context->scroll_panel.slot_area = Lui_context::SLOT_PANEL;
    
    context->elem_flags[Lui_context::SLOT_PANEL]  |= Lui_context::DRAW_AREA;
    context->elem_flags[Lui_context::SLOT_CANVAS] |= Lui_context::DRAW_AREA;

    context->resizers = array_create<Resizer>(Lui_context::RESIZER_COUNT);
    context->resizers[Lui_context::RESIZER_CREATE].slot = Lui_context::SLOT_RESIZER_CREATE;
    context->resizers[Lui_context::RESIZER_CREATE].size_min = (s64)std::round(context->fonts[Lui_context::FONT_LUI_NORMAL].newline) * 3;
    
    // Initialise application
    application_init();

    // Set initial radiobutton
    context->elem_flags[Lui_context::SLOT_LABEL_UNION] |= Lui_context::DRAW_PRESSED;

}

void lui_entry_clear(Lui_context* context, Text_entry* entry) {
    entry->cursor = 0;
    entry->cursor_row = 0;
    entry->cursor_col = 0;
    entry->text.size = 0;
    entry->selection = -1;
    entry->undo_stack.size = 0;
    entry->undo_data.size = 0;
    entry->redo_stack.size = 0;
    entry->redo_data.size = 0;
}

void _platform_operations_able(bool set) {
    Lui_context* context = &global_platform.gl_context;
        
    s64 elem_disable[] = {
        Lui_context::SLOT_BUTTON_DESC_OP,
        Lui_context::SLOT_BUTTON_DESC_REMOVEALL,
        Lui_context::SLOT_BUTTON_DESC_CONTEXT, 
        Lui_context::SLOT_LABEL_OPERATION,
        Lui_context::SLOT_LABEL_UNION,
        Lui_context::SLOT_LABEL_INTERSECTION,
        Lui_context::SLOT_LABEL_COMPLEMENT,
        Lui_context::SLOT_LABEL_FIRSTNODE,
        Lui_context::SLOT_LABEL_SECONDNODE,
        Lui_context::SLOT_BUTTON_OP,
        Lui_context::SLOT_BUTTON_REMOVEALL,
        Lui_context::SLOT_BUTTON_PREV,
        Lui_context::SLOT_BUTTON_NEXT,
        Lui_context::SLOT_ENTRY_FIRSTNODE,
        Lui_context::SLOT_ENTRY_SECONDNODE,
        Text_fmt::SLOT_CONTEXT,
        Text_fmt::SLOT_CONTEXT_FRAME
    };

    for (s64 i: elem_disable) {
        if (set) {
            context->elem_flags[i] |= Lui_context::DRAW_DISABLED;
            context->elem_flags[i] &= ~Lui_context::DRAW_FOCUSED;
            if (i == context->elem_focused) context->elem_focused = 0;
        } else {
            context->elem_flags[i] &= ~Lui_context::DRAW_DISABLED;
        }
    }
}
void platform_operations_disable() {
    _platform_operations_able(true);
}
void platform_operations_enable(u32 bdd) {
    if (bdd > 1) {
        auto context = &global_platform.gl_context;

        auto set_entry_bdd = [context](u8 entry_id, u32 bdd) {
            Text_entry* entry = &context->entries[entry_id];
            lui_entry_clear(context, entry);
            if (bdd > 1) {
                array_printf(&entry->text, "%d", bdd);
            } else {
                array_printf(&entry->text, "%s", bdd ? "F" : "T");
            }
        };

        set_entry_bdd(Lui_context::ENTRY_FIRSTNODE,  global_store.bdd_data[bdd].child0);
        set_entry_bdd(Lui_context::ENTRY_SECONDNODE, global_store.bdd_data[bdd].child1);
    }
    
    _platform_operations_able(false);
}

void platform_ui_button_help () {
    global_platform.gl_context.helptext_active ^= 1;
    platform_fmt_store_copy(Lui_context::SLOT_BUTTON_HELP, global_platform.gl_context.helptext_active
        ? Lui_context::SLOT_LABEL_HELP_HIDE : Lui_context::SLOT_LABEL_HELP_SHOW);
}
bool platform_ui_help_active () {
    return global_platform.gl_context.helptext_active;
}

Array_t<u8> platform_ui_value_get(u8 elem) {
    auto context = &global_platform.gl_context;
    
    switch (elem) {
    case Ui_elem::OP_NODE0: return context->entries[Lui_context::ENTRY_FIRSTNODE].text;
    case Ui_elem::OP_NODE1: return context->entries[Lui_context::ENTRY_SECONDNODE].text;
    case Ui_elem::CREATE_NUMS:  return context->entries[Lui_context::ENTRY_NUMBERS].text;
    case Ui_elem::CREATE_FORM:  return context->entries[Lui_context::ENTRY_FORMULA].text;
    case Ui_elem::CREATE_BASE:  return context->entries[Lui_context::ENTRY_BASE].text;
    case Ui_elem::CREATE_BITS:  return context->entries[Lui_context::ENTRY_BITORDER].text;
    case Ui_elem::CREATE_VARS:  return context->entries[Lui_context::ENTRY_VARORDER].text;
    case Ui_elem::OPERATION: {
        char const* c = "u";
        if (context->elem_flags[Lui_context::SLOT_LABEL_UNION]        & Lui_context::DRAW_PRESSED) c = "u";
        if (context->elem_flags[Lui_context::SLOT_LABEL_INTERSECTION] & Lui_context::DRAW_PRESSED) c = "i";
        if (context->elem_flags[Lui_context::SLOT_LABEL_COMPLEMENT]   & Lui_context::DRAW_PRESSED) c = "c";
        return {(u8*)c, 1};
    }
    case Ui_elem::CREATE_TYPE: {
        char const* c = "n";
        if (context->elem_flags[Lui_context::SLOT_LABEL_NUMBERS] & Lui_context::DRAW_PRESSED) c = "n";
        if (context->elem_flags[Lui_context::SLOT_LABEL_FORMULA] & Lui_context::DRAW_PRESSED) c = "f";
        return {(u8*)c, 1};
    }
    default: assert(false);
    }
}
void platform_ui_value_free(Array_t<u8> data) {}

void platform_ui_cursor_set(u8 elem, s64 cursor, s64 cursor_row, s64 cursor_col, s64 _cursor_char) {
    auto context = &global_platform.gl_context;
    
    Text_entry* entry;
    switch (elem) {
    case Ui_elem::OP_NODE0:     entry = &context->entries[Lui_context::ENTRY_FIRSTNODE]; break;
    case Ui_elem::OP_NODE1:     entry = &context->entries[Lui_context::ENTRY_SECONDNODE]; break;
    case Ui_elem::CREATE_NUMS:  entry = &context->entries[Lui_context::ENTRY_NUMBERS]; break;
    case Ui_elem::CREATE_FORM:  entry = &context->entries[Lui_context::ENTRY_FORMULA]; break;
    case Ui_elem::CREATE_BASE:  entry = &context->entries[Lui_context::ENTRY_BASE]; break;
    case Ui_elem::CREATE_BITS:  entry = &context->entries[Lui_context::ENTRY_BITORDER]; break;
    case Ui_elem::CREATE_VARS:  entry = &context->entries[Lui_context::ENTRY_VARORDER]; break;
    default: assert(false);
    };

    assert(0 <= cursor and cursor <= entry->text.size);
    entry->cursor = cursor;
    entry->cursor_row = cursor_row;
    entry->cursor_col = cursor_col;
    context->elem_flags[context->elem_focused] &= ~Lui_context::DRAW_FOCUSED;
    context->elem_focused = entry->slot;
    context->elem_flags[context->elem_focused] |=  Lui_context::DRAW_FOCUSED;
}

void platform_mouse_position(float* out_x, float* out_y) {
    auto context = &global_platform.gl_context;
    auto c = &global_context;

    float world_x = c->origin_x + (context->pointer_x - c->canvas_x) / c->scale;
    float world_y = c->origin_y + (c->height-1 - context->pointer_y + c->canvas_y) / c->scale;

    if (out_x) *out_x = world_x;
    if (out_y) *out_y = world_y;
}

void _platform_frame_draw() {
    float ox = global_context.screen_w / 2.f;
    float oy = global_context.screen_h / 2.f;
    float sx =  2.f / global_context.screen_w;
    float sy = -2.f / global_context.screen_h;

    auto context = &global_platform.gl_context; // The macros expect a local named context

    if (context->prep_ui.dirty) {
        context->prep_ui.dirty = false;
        
        if (context->uitext_tex) {
            glDeleteTextures(1, &context->uitext_tex);
        }
        glGenTextures(1, &context->uitext_tex);
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D, context->uitext_tex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, context->prep_ui.size, context->prep_ui.size, 0,
            GL_RED, GL_UNSIGNED_BYTE, context->prep_ui.image.data);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    }
    
    glEnable(GL_DEPTH_TEST);
    
    glUseProgram(context->program_uirect);
    
    OBST_DO_UNIFORM(UIRECT_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(UIRECT_SCALE, 2f, sx, sy);

    OBST_DO_BUFFER(uirect, UIRECT_POS,  buf_uirect_pos,  3, GL_FLOAT, 0);
    OBST_DO_BUFFER(uirect, UIRECT_FILL, buf_uirect_fill, 4, GL_UNSIGNED_BYTE, 1);

    glDrawArrays(GL_TRIANGLES, 0, context->buf_uirect_pos.size / 3);

    glUseProgram(context->program_uibutton);

    OBST_DO_UNIFORM(UIBUTTON_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(UIBUTTON_SCALE, 2f, sx, sy);

    OBST_DO_BUFFER(uibutton, UIBUTTON_POS,    buf_uibutton_pos,    2, GL_FLOAT, 0);
    OBST_DO_BUFFER(uibutton, UIBUTTON_X,      buf_uibutton_x,      2, GL_FLOAT, 0);
    OBST_DO_BUFFER(uibutton, UIBUTTON_SIZE,   buf_uibutton_size,   1, GL_FLOAT, 0);
    OBST_DO_BUFFER(uibutton, UIBUTTON_BORDER, buf_uibutton_border, 3, GL_FLOAT, 0);
    OBST_DO_BUFFER(uibutton, UIBUTTON_COLOR,  buf_uibutton_color,  3, GL_FLOAT, 0);
    OBST_DO_BUFFER(uibutton, UIBUTTON_GRAD,   buf_uibutton_grad,   4, GL_FLOAT, 0);
    OBST_DO_BUFFER(uibutton, UIBUTTON_CIRCLE, buf_uibutton_circle, 2, GL_FLOAT, 0);

    glDrawArrays(GL_TRIANGLE_STRIP, 0, context->buf_uibutton_pos.size / 2);
    
    glUseProgram(context->program_uitext);
    glActiveTexture(GL_TEXTURE1);

    OBST_DO_UNIFORM(UITEXT_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(UITEXT_SCALE, 2f, sx, sy);
    OBST_DO_UNIFORM(UITEXT_SAMPLER, 1i, 1);

    OBST_DO_BUFFER(uitext, UITEXT_POS,  buf_uitext_pos,  2, GL_FLOAT, 0);
    OBST_DO_BUFFER(uitext, UITEXT_TPOS, buf_uitext_tpos, 2, GL_FLOAT, 0);
    OBST_DO_BUFFER(uitext, UITEXT_FILL, buf_uitext_fill, 4, GL_UNSIGNED_BYTE, 1);

    glDrawArrays(GL_TRIANGLES, 0, context->buf_uitext_pos.size / 2);
}

// Debug funtionality
void _print_undo_stack(Array_t<Undo_item> undo_stack, Array_t<u8> undo_data) {
    for (s64 it = 0; it < undo_stack.size; ++it) {
        Undo_item i = undo_stack[it];
        if (i.type == Undo_item::INSERT) {
            printf("%2lld ins ins_beg=%lld ins_off=%lld flags=", it, i.ins_beg, i.ins_offset);
            bool sep = false;
            if (i.flags & Undo_item::COMMITTED) {printf("%scom", sep ? "|" : ""); sep = true;}
            if (i.flags & Undo_item::REVERSED) {printf("%srev", sep ? "|" : ""); sep = true;}
            if (i.flags & Undo_item::GROUPED) {printf("%sgrp", sep ? "|" : ""); sep = true;}
            printf(" data=");
            s64 end = undo_data.size;
            for (s64 j = it+1; j < undo_stack.size; ++j) {
                if (undo_stack[j].type == Undo_item::INSERT) {
                    end = undo_stack[j].ins_offset; break;
                }
            }
            for (u8 c: array_subarray(undo_data, i.ins_offset, end)) printf("%c", (char)c);
            puts("");
        } else if (i.type == Undo_item::DELETE) {
            printf("%2lld del del_beg=%lld del_end=%lld flags=", it, i.del_beg, i.del_end);
            bool sep = false;
            if (i.flags & Undo_item::COMMITTED) {printf("com%s", sep ? "|" : ""); sep = true;}
            if (i.flags & Undo_item::REVERSED) {printf("rev%s", sep ? "|" : ""); sep = true;}
            if (i.flags & Undo_item::GROUPED) {printf("grp%s", sep ? "|" : ""); sep = true;}
            puts("");
        } else {
            printf("%2lld none\n", it);
        }
    }
};

bool _lui_process_key_entry(Lui_context* context, Text_entry* entry, Key key) {
    enum Move_mode: u8 {
        SINGLE, LINE, WORD, FOREVER
    };
    enum Move_dir: u8 {
        MOVE_R, MOVE_L, MOVE_D, MOVE_U
    };
    enum Undo_mode: u8 {
        USE_REDO_STACK = 1, RETAIN_REDO = 2
    };

    auto isalnum = [](u8 c) {
        return ('0' <= c and c <= '9') or ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z');
    };
    auto reset_selection = [entry](bool shift) {
        if (shift) {
            if (entry->selection == -1) {
                entry->selection = entry->cursor;
                entry->selection_row = entry->cursor_row;
                entry->selection_col = entry->cursor_col;
            }
        } else {
            entry->selection = -1;
        }
        
    };
    auto move_r = [entry, &isalnum, &reset_selection](u8 mode, bool shift) {
        reset_selection(shift);
        
        while (entry->cursor < entry->text.size) {
            if (entry->text[entry->cursor] == '\n') {
                if (mode == LINE) break;
                entry->cursor_col = -1;
                ++entry->cursor_row;
            }
            do {
                ++entry->cursor_col; ++entry->cursor;
            } while (entry->cursor < entry->text.size and (entry->text[entry->cursor] >> 6) == 2);
            if (mode == SINGLE) break;
            if (mode == WORD and entry->cursor < entry->text.size and isalnum(entry->text[entry->cursor-1])
                and not isalnum(entry->text[entry->cursor])) break;
        }
    };
    auto move_l = [entry, &isalnum, &reset_selection](u8 mode, bool shift) {
        reset_selection(shift);
        
        while (entry->cursor > 0) {
            if (mode == LINE and entry->text[entry->cursor-1] == '\n') break;
            
            do {
                --entry->cursor_col; --entry->cursor;
            } while (entry->cursor < entry->text.size and (entry->text[entry->cursor] >> 6) == 2);
            
            if (entry->text[entry->cursor] == '\n') {
                assert(entry->cursor_col < 0);
                --entry->cursor_row;
            }
            if (mode == SINGLE) break;
            if (mode == WORD and entry->cursor > 0 and not isalnum(entry->text[entry->cursor-1])
                and isalnum(entry->text[entry->cursor])) break;
        }
        if (entry->cursor_col < 0) {
            entry->cursor_col = 0;
            while (true) {
                if (entry->cursor - entry->cursor_col - 1 < 0) break;
                if (entry->text[entry->cursor - entry->cursor_col - 1] == '\n') break;
                ++entry->cursor_col;
            }
        }
    };
    
    auto get_width = [context, entry]() {
        s64 width = 0;
        for (s64 c_i = entry->cursor - entry->cursor_col; c_i < entry->cursor;) {
            s64 c_len = helper_decode_utf8(array_subarray(entry->text, c_i, entry->text.size));
            Text_box box;

            // This should basicall just do a lookup
            lui_text_prepare_word(context, &context->prep_ui, Lui_context::FONT_LUI_BUTTON,
                array_subarray(entry->text, c_i, c_i+c_len), &box);
            
            width += (s64)std::round(box.advance);
            c_i += c_len;
        }
        return width;
    };
    auto move_width = [context, entry](s64 width) {
        while (entry->cursor < entry->text.size and width > 0) {
            if (entry->text[entry->cursor] == '\n') break;
            
            s64 c_len = helper_decode_utf8(array_subarray(entry->text, entry->cursor, entry->text.size));
            Text_box box;
            auto c_arr = array_subarray(entry->text, entry->cursor, entry->cursor+c_len);

            // This should basicall just do a lookup
            lui_text_prepare_word(context, &context->prep_ui, Lui_context::FONT_LUI_BUTTON, c_arr, &box);

            s64 advance = (s64)std::round(box.advance);
            if (advance >= 2 * width) break;

            entry->cursor += c_len;
            entry->cursor_col += c_len;
            width -= advance;
        }
    };

    auto move_row = [context, entry](s64 amount) {
        if (amount > 0) {
            while (entry->cursor < entry->text.size and amount > 0) {
                if (entry->text[entry->cursor] == '\n') {
                    --amount;
                    ++entry->cursor_row;
                    entry->cursor_col = -1;
                }
                ++entry->cursor;
                ++entry->cursor_col;
            }
        } else if (amount < 0) {
            amount = -amount;
            while (entry->cursor > 0 and amount > 0) {
                --entry->cursor;
            
                if (entry->text[entry->cursor] == '\n') {
                    --amount;
                    --entry->cursor_row;
                }
            }
            while (entry->cursor > 0 and entry->text[entry->cursor-1] != '\n') {
                --entry->cursor;
            }
            entry->cursor_col = 0;
        }
        return amount == 0;
    };    
    auto move_du = [entry, &get_width, &move_row, &move_width, &reset_selection](s64 amount, bool shift) {
        reset_selection(shift);
        s64 width = get_width();
        if (move_row(amount)) move_width(width);
    };
    auto move_jump = [entry](s64 i) {
        entry->cursor_row = 0;
        entry->cursor_col = 0;
        entry->selection = -1;
        for (entry->cursor = 0; entry->cursor < i; ++entry->cursor) {
            ++entry->cursor_col;
            if (entry->text[entry->cursor] == '\n') {
                ++entry->cursor_row;
                entry->cursor_col = 0;
            }
        }
    };
    
    auto del = [entry, move_l, move_r](u8 dir, u8 mode, u8 undo_mode=0) {
        s64 i = entry->cursor;
        s64 i_row = entry->cursor_row;
        s64 i_col = entry->cursor_col;
        bool commit = false;

        if (entry->selection != -1) {
            entry->cursor = entry->selection;
            entry->cursor_row = entry->selection_row;
            entry->cursor_col = entry->selection_col;
            entry->selection = -1;
            commit = true;
        } else {
            if      (dir == MOVE_L) move_l(mode, false);
            else if (dir == MOVE_R) move_r(mode, false);
            else assert(false);
        }
        s64 j = entry->cursor;

        if (i < j) {
            entry->cursor = i;
            entry->cursor_row = i_row;
            entry->cursor_col = i_col;
        } else if (i > j) {
            std::swap(i, j);
        } else {
            return;
        }

        if (~undo_mode & RETAIN_REDO) {
            entry->redo_stack.size = 0;
            entry->redo_data.size = 0;
        }
        auto* undo_stack = ~undo_mode & USE_REDO_STACK ? &entry->undo_stack : &entry->redo_stack;
        auto* undo_data  = ~undo_mode & USE_REDO_STACK ? &entry->undo_data  : &entry->redo_data;
            
        bool consumed_undo = false;
        if (undo_stack->size) {
            auto* last = &(*undo_stack)[undo_stack->size-1];
            bool maybe_update = last->type == Undo_item::INSERT and (~last->flags & Undo_item::COMMITTED);
            if (maybe_update and last->ins_beg == i and (~last->flags & Undo_item::REVERSED)) {
                array_append(undo_data, array_subarray(entry->text, i, j));
                consumed_undo = true;
            } else if (maybe_update and last->ins_beg == j and (last->flags & Undo_item::REVERSED)) {
                array_resize(undo_data, undo_data->size + j-i);
                for (s64 k = 0; k < j-i; ++k) {
                    (*undo_data)[undo_data->size-1 - k] = entry->text[i + k];
                }
                last->ins_beg -= j - i;
                consumed_undo = true;
            } else {
                last->flags |= Undo_item::COMMITTED;
            }
        }
            
        if (not consumed_undo) {
            Undo_item undo {Undo_item::INSERT, 0};
            if (commit) undo.flags |= Undo_item::COMMITTED;
            undo.ins_beg = i;
            undo.ins_offset = undo_data->size;
            if (dir == MOVE_R) {
                array_append(undo_data, array_subarray(entry->text, i, j));
            } else {
                undo.flags |= Undo_item::REVERSED;
                array_resize(undo_data, undo_data->size + j-i);
                for (s64 k = 0; k < j-i; ++k) {
                    (*undo_data)[undo_data->size-1 - k] = entry->text[i + k];
                }
            }
            array_push_back(undo_stack, undo);
        }
        
        memmove(entry->text.data + i, entry->text.data + j, entry->text.size - j);
        entry->text.size -= j - i;
    };
    auto ins = [entry, del](Array_t<u8> text, u8 undo_mode=0) {
        if (entry->selection != -1) {
            del(0, 0);
        }
        
        s64 i = entry->cursor;
        array_reserve(&entry->text, entry->text.size + text.size);
        memmove(entry->text.data + i + text.size, entry->text.data + i, entry->text.size - i);
        memcpy(entry->text.data + i, text.data, text.size);
        entry->text.size += text.size;
        entry->cursor += text.size;
        for (u8 c: text) {
            entry->cursor_col += 1;
            if (c == '\n') {
                entry->cursor_col = 0;
                entry->cursor_row += 1;
                
            }
        }

        if (~undo_mode & RETAIN_REDO) {
            entry->redo_stack.size = 0;
            entry->redo_data.size = 0;
        }
        auto* undo_stack = ~undo_mode & USE_REDO_STACK ? &entry->undo_stack : &entry->redo_stack;
        auto* undo_data  = ~undo_mode & USE_REDO_STACK ? &entry->undo_data  : &entry->redo_data;
        
        s64 s = undo_stack->size;
        if (s and (*undo_stack)[s-1].type == Undo_item::DELETE
            and (~(*undo_stack)[s-1].flags & Undo_item::COMMITTED)
            and (*undo_stack)[s-1].del_end == i)
        {
            (*undo_stack)[s-1].del_end += text.size;
        } else {
            if (s) {
                (*undo_stack)[s-1].flags |= Undo_item::COMMITTED;
            }
            Undo_item undo {Undo_item::DELETE, 0};
            undo.del_beg = i;
            undo.del_end = i + text.size;
            array_push_back(undo_stack, undo);
        }
    };
    
    auto undo_or_redo = [entry, move_jump, ins, del](bool undo) {
        auto* undo_stack = undo ? &entry->undo_stack : &entry->redo_stack;
        auto* undo_data  = undo ? &entry->undo_data  : &entry->redo_data;
        u8 undo_mode = undo ? USE_REDO_STACK : 0;
        
        if (undo_stack->size == 0) return;

        (*undo_stack)[undo_stack->size-1].flags |= Undo_item::COMMITTED;

        Undo_item i = (*undo_stack)[undo_stack->size - 1];
        if (i.type == Undo_item::INSERT) {
            auto arr = array_subarray(*undo_data, i.ins_offset, undo_data->size);
            if (i.flags & Undo_item::REVERSED) {
                for (s64 k = 0; 2*k < arr.size; ++k) {
                    std::swap(arr[k], arr[arr.size-1 - k]);
                }
            }
            move_jump(i.ins_beg);
            ins(arr, undo_mode | RETAIN_REDO);
            undo_data->size = i.ins_offset;
            --undo_stack->size;
        } else if (i.type == Undo_item::DELETE) {
            move_jump(i.del_beg);
            entry->selection = i.del_end; // Note: selection_row and _col are irrelevant here, leave them
            del(0, 0, undo_mode | RETAIN_REDO);
            --undo_stack->size;
        } else {
            assert(false);
        }
    };
    
    bool consumed = true;
    bool commit = true;
    if (key.type == Key::SPECIAL) {
        if (key.special == Key::ARROW_R) {
            move_r(key.flags & Key::MOD_CTRL ? WORD : SINGLE, key.flags & Key::MOD_SHIFT);
        } else if (key.special == Key::ARROW_L) {
            move_l(key.flags & Key::MOD_CTRL ? WORD : SINGLE, key.flags & Key::MOD_SHIFT);
        } else if (key.special == Key::ARROW_D and (~key.flags & Key::MOD_CTRL)) {
            move_du( 1, key.flags & Key::MOD_SHIFT);
        } else if (key.special == Key::ARROW_U and (~key.flags & Key::MOD_CTRL)) {
            move_du(-1, key.flags & Key::MOD_SHIFT);
        } else if (key.special == Key::HOME) {
            move_l(key.flags & Key::MOD_CTRL ? FOREVER : LINE, key.flags & Key::MOD_SHIFT);
        } else if (key.special == Key::END) {
            move_r(key.flags & Key::MOD_CTRL ? FOREVER : LINE, key.flags & Key::MOD_SHIFT);

        } else if (key.special == Key::DELETE) {
            del(MOVE_R, key.flags & Key::MOD_CTRL ? WORD : SINGLE); commit = false;
        } else if (key.special == Key::BACKSPACE) {
            del(MOVE_L, key.flags & Key::MOD_CTRL ? WORD : SINGLE); commit = false;

        } else if (key.special == Key::RETURN) {
            if (not entry->disable_newline)
                { ins({(u8*)"\n", 1}); commit = false; }

        } else if (key.special == Key::C_PASTE) {
            ins(platform_clipboard_get(key.data));
            platform_clipboard_free(key.data);
        } else if (key.special == Key::C_COPY and entry->selection != -1) {
            s64 i = entry->cursor, j = entry->selection;
            if (i > j) std::swap(i, j);
            platform_clipboard_set(Platform_clipboard::CONTROL_C, array_subarray(entry->text, i, j));
        } else if (key.special == Key::C_CUT and entry->selection != -1) {
            s64 i = entry->cursor, j = entry->selection;
            if (i > j) std::swap(i, j);
            platform_clipboard_set(Platform_clipboard::CONTROL_C, array_subarray(entry->text, i, j));
            del(0, 0);
            
        } else if (key.special == Key::C_SELECTALL) {
            move_jump(0);
            // Note that selection_row and _col are only relevant when the selection is before the cursor
            entry->selection = entry->text.size;
        } else if (key.special == Key::C_UNDO) {
            undo_or_redo(true); commit = false;
        } else if (key.special == Key::C_REDO) {
            undo_or_redo(false); commit = false;
            
        } else {
            consumed = false;
        }
        
    } else if (key.type == Key::TEXT) {
        ins({(u8*)key.text, (s64)strlen((char*)key.text)}); commit = false;
    } else if (key.type == Key::MOUSE) {
        u8 action; s64 x, y;
        key.get_mouse_param(&action, &x, &y);

        bool shift;
        if (action == Key::LEFT_DOWN) {
            shift = false;
        } else if (action == Key::MOTION and context->drag_el == entry->slot) {
            shift = true;
        } else {
            consumed = false;
        }

        if (action == Key::SCROLL_DOWN or action == Key::SCROLL_UP) {
            s64 diff = (action == Key::SCROLL_DOWN) - (action == Key::SCROLL_UP);
            s64 line = (s64)std::round(context->fonts[Lui_context::FONT_LUI_BUTTON].newline);
            entry->offset_y += diff * line;

            s64 total_rows = 1;
            for (u8 c: entry->text) total_rows += c == '\n';
            entry->offset_y = std::min(entry->offset_y, total_rows*line - entry->draw_h);
            entry->offset_y = std::max(entry->offset_y, (s64)0);
        }

        if (action == Key::LEFT_UP and context->drag_el == entry->slot and entry->selection != -1) {
            // Note that we receive this event even if the cursor is not over the entry
            s64 i = entry->cursor, j = entry->selection;
            if (i > j) std::swap(i, j);
            platform_clipboard_set(Platform_clipboard::MIDDLE_BUTTON, array_subarray(entry->text, i, j));
        }

        if (consumed) {
            Rect bb = context->elem_bb[entry->slot];
            Padding pad = context->padding_entry;
            auto font_inst = context->fonts[Lui_context::FONT_LUI_BUTTON];
                
            s64 tx = x - bb.x - pad.mar_x - pad.pad_x + entry->offset_x;
            s64 ty = y - bb.y - pad.mar_y - pad.pad_y + entry->offset_y;
            s64 row = (s64)std::floor(ty / font_inst.newline);

            reset_selection(shift);
            entry->cursor -= entry->cursor_col;
            entry->cursor_col = 0;
            move_row(row - entry->cursor_row);
            move_width(tx);
        }
    } else {
        consumed = false;
    }

    if (commit and entry->undo_stack.size) {
        entry->undo_stack[entry->undo_stack.size-1].flags |= Undo_item::COMMITTED;
    }
    
    if (consumed) {
        // Adjust offset, to put cursor in view
        s64 width = get_width();
        if (width < entry->offset_x) {
            entry->offset_x = width;
        } else if (width > entry->offset_x + entry->draw_w) {
            entry->offset_x = width - entry->draw_w;
        }
        s64 line = (s64)std::round(context->fonts[Lui_context::FONT_LUI_BUTTON].newline);
        s64 height = entry->cursor_row * line;
        if (height < entry->offset_y) {
            entry->offset_y = height;
        } else if (height + line > entry->offset_y + entry->draw_h) {
            entry->offset_y = height+line - entry->draw_h;
        }

        // Reset blinking cycle
        context->cursor_next_blink = platform_now() + Lui_context::CURSOR_BLINK_DELAY;
        context->cursor_blinked = false;
    }

    return consumed;
}

void lui_button_press(s64 slot) {
    switch (slot) {
    case Lui_context::SLOT_BUTTON_CREATE:     ui_button_create(); break;
    case Lui_context::SLOT_BUTTON_OP:         ui_button_op(); break;
    case Lui_context::SLOT_BUTTON_REMOVEALL:  ui_button_removeall(); break;
    case Lui_context::SLOT_BUTTON_NEXT:       ui_button_move(1.f); break;
    case Lui_context::SLOT_BUTTON_PREV:       ui_button_move(-1.f); break;
    case Lui_context::SLOT_BUTTON_HELP:       platform_ui_button_help(); break;
    case Lui_context::SLOT_BUTTON_HELP_CLOSE: platform_ui_button_help(); break;
    default: assert(false);
    }
}

void lui_radio_press(s64 slot) {
    if (slot == Lui_context::SLOT_LABEL_UNION) {
        platform_fmt_store_copy(Lui_context::SLOT_BUTTON_OP, Lui_context::SLOT_LABEL_OP_U);
    } else if (slot == Lui_context::SLOT_LABEL_INTERSECTION) {
        platform_fmt_store_copy(Lui_context::SLOT_BUTTON_OP, Lui_context::SLOT_LABEL_OP_I);
    } else if (slot == Lui_context::SLOT_LABEL_COMPLEMENT) {
        platform_fmt_store_copy(Lui_context::SLOT_BUTTON_OP, Lui_context::SLOT_LABEL_OP_C);
    }
}

void _platform_render(Platform_state* platform) {
    assert(platform);
    
    Lui_context* context = &global_platform.gl_context;
    auto font_inst = context->fonts[Lui_context::FONT_LUI_NORMAL];

    // Process input
    
    auto in_rect = [](Rect r, s64 x, s64 y) {
        return r.x <= x and x < r.x+r.w and r.y <= y and y < r.y+r.h;
    };
    auto get_entry = [context](s64 id) {
        for (Text_entry& i: context->entries) {
            if (i.slot == id) return &i;
        }
        assert(false);
    };

    auto get_scroll = [context](s64 slot) {
        assert(slot == Lui_context::SLOT_SCROLL_PANEL);
        return &context->scroll_panel;
    };
    auto get_scroll_from_area = [context](s64 slot_area) -> Scrollbar* {
        if (context->scroll_panel.slot_area == slot_area) {
            return &context->scroll_panel;
        }
        return nullptr;
    };
    
    auto scrollbar_scroll = [context](Scrollbar* scroll, float amount) {
        s64 h = context->elem_bb[scroll->slot_area].h;
        if (scroll->offset == 0 and scroll->total_height <= h) return;

        animation_add(&scroll->anim, amount, 0.f, (float)(scroll->total_height - h));
    };
    auto scrollbar_scroll_set = [context](Scrollbar* scroll, s64 offset) {
        s64 h = context->elem_bb[scroll->slot_area].h;
        if (scroll->offset == 0 and scroll->total_height <= h) return;

        animation_set(&scroll->anim, (float)offset, 0.f, (float)(scroll->total_height - h));
    };

    auto get_resizer = [context](s64 id) {
        for (auto& i: context->resizers) {
            if (i.slot == id) return &i;
        }
        assert(false);
    };

    for (Key key: context->input_queue) {
        if (key.type == Key::MOUSE) {
            u8 action; s64 x, y;
            key.get_mouse_param(&action, &x, &y);

            bool consumed = false;

            if (action == Key::MOTION and context->drag_el != -1) {
                if (context->elem_flags[context->drag_el] & Lui_context::DRAW_SCROLL) {
                    Scrollbar* scroll = get_scroll(context->drag_el);
                    s64 amount = (y - scroll->drag_y) * scroll->total_height / context->elem_bb[scroll->slot_area].h;
                    scrollbar_scroll_set(scroll, scroll->drag_offset + amount);
                    consumed = true;
                } else if (context->elem_flags[context->drag_el] & Lui_context::DRAW_RESIZER) {
                    Resizer* resize = get_resizer(context->drag_el);
                    resize->size = resize->drag_size + (y - resize->drag_y);
                    consumed = true;
                }
            }

            if (consumed) continue;
            
            u8 cursor_type = Cursor_type::NORMAL;
            for (s64 slot = 0; slot < Lui_context::SLOT_COUNT; ++slot) {
                if (context->elem_flags[slot] & Lui_context::DRAW_DISABLED) continue;
                
                if (not in_rect(context->elem_bb[slot], x, y)) {
                    context->elem_flags[slot] &= ~Lui_context::DRAW_ACTIVE;
                    // This is a bit hacky. Due to the way we only get the correct flags after drawing once,
                    // we run the risk of resetting the initial radiobutton selection here.
                    if (context->elem_flags[slot] & (Lui_context::DRAW_BUTTON | Lui_context::DRAW_ENTRY)) {
                        context->elem_flags[slot] &= ~Lui_context::DRAW_PRESSED;
                    }
                    continue;
                }

                if (consumed) continue;
                
                if (action == Key::LEFT_DOWN) {
                    context->elem_flags[context->elem_focused] &= ~Lui_context::DRAW_FOCUSED;
                    context->elem_focused = slot;
                    context->elem_tabindex = slot;
                    context->elem_flags[slot] |= Lui_context::DRAW_PRESSED;
                    context->elem_flags[slot] |= Lui_context::DRAW_FOCUSED;
                    context->drag_el = slot;

                    if (context->elem_flags[slot] & Lui_context::DRAW_RADIO) {
                        for (s64 slot_j = slot-1; context->elem_flags[slot_j] & Lui_context::DRAW_RADIO; --slot_j)
                            context->elem_flags[slot_j] &= ~Lui_context::DRAW_PRESSED;
                        for (s64 slot_j = slot+1; context->elem_flags[slot_j] & Lui_context::DRAW_RADIO; ++slot_j)
                            context->elem_flags[slot_j] &= ~Lui_context::DRAW_PRESSED;
                    } else if (context->elem_flags[slot] & Lui_context::DRAW_ENTRY) {
                        context->cursor_next_blink = platform_now() + Lui_context::CURSOR_BLINK_DELAY;
                        context->cursor_blinked = false;
                        _lui_process_key_entry(context, get_entry(slot), key);
                    } else if (context->elem_flags[slot] & Lui_context::DRAW_SCROLL) {
                        Scrollbar* scroll = get_scroll(slot);
                        scroll->drag_offset = scroll->offset;
                        scroll->drag_y = y;
                    } else if (context->elem_flags[slot] & Lui_context::DRAW_RESIZER) {
                        Resizer* resize = get_resizer(slot);
                        resize->drag_size = resize->size;
                        resize->drag_y = y;
                    }
                } else if (action == Key::LEFT_UP) {
                    bool pressed = context->elem_flags[slot] & Lui_context::DRAW_PRESSED;
                    if (~context->elem_flags[slot] & Lui_context::DRAW_RADIO) {
                        context->elem_flags[slot] &= ~Lui_context::DRAW_PRESSED;
                    }
                    if (pressed and (context->elem_flags[slot] & Lui_context::DRAW_BUTTON)) {
                        lui_button_press(slot);
                    } else if (pressed and (context->elem_flags[slot] & Lui_context::DRAW_RADIO)) {
                        lui_radio_press(slot);
                    }
                } else if (action == Key::MOTION) {
                    if (context->drag_el == slot and (context->elem_flags[slot] & Lui_context::DRAW_BUTTON)) {
                        context->elem_flags[slot] |= Lui_context::DRAW_PRESSED;
                    }
                    
                    context->elem_flags[slot] |= Lui_context::DRAW_ACTIVE;
                    if (context->elem_flags[slot] & Lui_context::DRAW_ENTRY) {
                        cursor_type = Cursor_type::TEXT;
                        _lui_process_key_entry(context, get_entry(slot), key);
                    } else if (slot == Lui_context::SLOT_CANVAS) {
                        auto c = &global_context;

                        float world_x = c->origin_x + (x - c->canvas_x) / c->scale;
                        float world_y = c->origin_y + (c->height-1 - y + c->canvas_y) / c->scale;
                        ui_mouse_move(world_x, world_y);
                    } else if (context->elem_flags[slot] & Lui_context::DRAW_RESIZER) {
                        cursor_type = Cursor_type::RESIZE;
                    }
                } else if (action == Key::SCROLL_DOWN or action == Key::SCROLL_UP) {
                    s64 diff = (action == Key::SCROLL_DOWN) - (action == Key::SCROLL_UP);
                    if (context->elem_flags[slot] & Lui_context::DRAW_AREA) {
                        Scrollbar* scroll = get_scroll_from_area(slot);
                        if (not scroll) continue;

                        float scroll_amount = context->fonts[Lui_context::FONT_LUI_NORMAL].newline * 3.f;
                        scrollbar_scroll(scroll, scroll_amount * (float)diff);
                    } else if (context->elem_flags[slot] & Lui_context::DRAW_ENTRY) {
                        _lui_process_key_entry(context, get_entry(slot), key);
                    } else {
                        continue;
                    }
                }
                
                consumed = true;
            }
            
            if (action == Key::MOTION) {
                platform_set_cursor(cursor_type);
            } else if (action == Key::LEFT_UP) {
                if (context->drag_el != -1 and context->elem_flags[context->drag_el] & Lui_context::DRAW_ENTRY) {
                    _lui_process_key_entry(context, get_entry(context->drag_el), key);
                }
                context->drag_el = -1;
            }

            if (not consumed and action == Key::LEFT_DOWN) {
                context->elem_flags[context->elem_focused] &= ~Lui_context::DRAW_FOCUSED;
                context->elem_focused = 0;
                consumed = true;
            }

            if (consumed) continue;
        } else if (key.type == Key::SPECIAL) {
            bool consumed = false;

            if (key.special == Key::C_QUIT) {
                exit(0);
            } else if (key.special == Key::TAB or key.special == Key::SHIFT_TAB) {
                if (context->elem_focused != 0 or context->elem_tabindex == 0) {
                    context->elem_flags[context->elem_focused] &= ~Lui_context::DRAW_FOCUSED;
                    s64 diff = (key.special == Key::TAB) - (key.special == Key::SHIFT_TAB)  + Lui_context::SLOT_COUNT;
                    while (true) {
                        context->elem_tabindex = (context->elem_tabindex + diff) % Lui_context::SLOT_COUNT;
                        u64 flags = context->elem_flags[context->elem_tabindex];
                        if (flags & Lui_context::DRAW_DISABLED) continue;
                        if (flags & (Lui_context::DRAW_ENTRY | Lui_context::DRAW_BUTTON)) break;
                        if ((flags & Lui_context::DRAW_RADIO) and (flags & Lui_context::DRAW_PRESSED)) break;
                    }
                }
                context->elem_focused = context->elem_tabindex;
                context->elem_flags[context->elem_focused] |= Lui_context::DRAW_FOCUSED;

                if (context->elem_flags[context->elem_focused] & Lui_context::DRAW_ENTRY) {
                    _lui_process_key_entry(context, get_entry(context->elem_focused), Key::create_special(Key::C_SELECTALL, 0));
                }
                
                consumed = true;
            }

            u64 flags = context->elem_flags[context->elem_focused];
            if (flags & Lui_context::DRAW_RADIO) {
                s64 diff = 0;
                if (key.special == Key::ARROW_R or key.special == Key::ARROW_D) {
                    diff = 1;
                } else if (key.special == Key::ARROW_L or key.special == Key::ARROW_U) {
                    diff = -1;
                }
                if (diff != 0) {
                    s64 slot = context->elem_focused + diff;
                    if (~context->elem_flags[slot] & Lui_context::DRAW_RADIO) {
                        while (context->elem_flags[slot-diff] & Lui_context::DRAW_RADIO) slot -= diff;
                    }
                    context->elem_flags[context->elem_focused] &= ~Lui_context::DRAW_PRESSED;
                    context->elem_flags[context->elem_focused] &= ~Lui_context::DRAW_FOCUSED;
                    context->elem_focused = slot;
                    context->elem_tabindex = slot;
                    context->elem_flags[slot] |= Lui_context::DRAW_PRESSED;
                    context->elem_flags[slot] |= Lui_context::DRAW_FOCUSED;

                    lui_radio_press(slot);
                    
                    consumed = true;
                }
            }

            if (consumed) continue;
        } else if (key.type == Key::GENERAL) {
            if (key.general == Key::FOCUS_IN) {
                context->window_has_focus = true;
            } else if (key.general == Key::FOCUS_OUT) {
                context->window_has_focus = false;
            }
        }

        if (key.type == Key::TEXT or key.type == Key::SPECIAL) {
            // Hacky: Reset the create helper if the user presses a key
            if (global_ui.novice_create_helper == 1) {
                global_ui.novice_create_helper = 2;
            }
        }

        if (context->elem_flags[context->elem_focused] & Lui_context::DRAW_ENTRY) {
            Text_entry* entry = get_entry(context->elem_focused);
            bool consumed = _lui_process_key_entry(context, entry, key);
            if (consumed) continue;
        }
        
        ui_key_press(key);
    }
    context->input_queue.size = 0;

    // If we are not in focus, draw everything unfocused
    if (not context->window_has_focus) {
        for (s64 slot = 0; slot < Lui_context::SLOT_COUNT; ++slot) {
            context->elem_flags[slot] &= ~Lui_context::DRAW_FOCUSED;
        }
    } else {
        context->elem_flags[context->elem_focused] |= Lui_context::DRAW_FOCUSED;
    }

    // Update the scrollbar
    Scrollbar* scroll = &context->scroll_panel;
    bool need_redraw;
    scroll->offset = (s64)std::round(animation_get(&scroll->anim, &need_redraw));
    if (need_redraw) platform_redraw(0);

    // Update the resizers
    for (auto& resizer_: context->resizers) {
        auto* resizer = &resizer_;
        resizer->size = std::max(resizer->size, resizer->size_min);
    }
    
    // Decide whether and where to draw the text cursor, normalise text entry data
    bool cursor_blinking = false;
    for (Text_entry& entry: context->entries) {
        u64 flags = context->elem_flags[entry.slot];
        if (flags & Lui_context::DRAW_FOCUSED) {
            cursor_blinking = true;
        } else {
            entry.cursor_draw = false;
        }
        if ((~flags & Lui_context::DRAW_FOCUSED) or entry.selection == entry.cursor) {
            entry.selection = -1;
        }
    }
    if (not context->window_has_focus) {
        cursor_blinking = false;
    }
    
    if (cursor_blinking) {
        if (context->cursor_next_blink == INFINITY) {
            context->cursor_next_blink = platform_now() + Lui_context::CURSOR_BLINK_DELAY;
            context->cursor_blinked = false;
        }
        if (platform_now() >= context->cursor_next_blink) {
            context->cursor_next_blink += Lui_context::CURSOR_BLINK_DELAY;
            context->cursor_blinked ^= 1;
        }
        platform_redraw(context->cursor_next_blink);
    } else {
        context->cursor_next_blink = INFINITY;
        context->cursor_blinked = false;
    }
    if (context->elem_flags[context->elem_focused] & Lui_context::DRAW_ENTRY) {
        get_entry(context->elem_focused)->cursor_draw = not context->cursor_blinked;
    }

    // Reset bbs
    for (s64 slot = 0; slot < Lui_context::SLOT_COUNT; ++slot) {
        context->elem_bb[slot] = Rect {};
    }

    // Determine whether we show the number input or the formula one
    bool create_type_numbers = context->elem_flags[Lui_context::SLOT_LABEL_NUMBERS] & Lui_context::DRAW_PRESSED;
    auto set_disabled = [context](s64 slot, bool value) {
        if (value) context->elem_flags[slot] |=  Lui_context::DRAW_DISABLED;
        else       context->elem_flags[slot] &= ~Lui_context::DRAW_DISABLED;
    };

    set_disabled(Lui_context::SLOT_ENTRY_FORMULA,   create_type_numbers);
    set_disabled(Lui_context::SLOT_ENTRY_BASE,     !create_type_numbers);
    set_disabled(Lui_context::SLOT_LABEL_BASE,     !create_type_numbers);
    set_disabled(Lui_context::SLOT_ENTRY_NUMBERS,  !create_type_numbers);
    set_disabled(Lui_context::SLOT_ENTRY_BITORDER, !create_type_numbers);
    set_disabled(Lui_context::SLOT_ENTRY_VARORDER,  create_type_numbers);
    
    // Draw the application
    application_render();
    context->elem_bb[Lui_context::SLOT_CANVAS] = Rect {
        global_context.canvas_x, global_context.canvas_y, (s64)global_context.width, (s64)global_context.height
    };

    _platform_frame_init();
    
    // Now draw the UI
    
    u8 white[] = {255, 255, 255, 255};
    u8 black[] = {0, 0, 0, 255};
    lui_draw_rect(context, 0, 0, context->panel_left_width, global_context.screen_h, Lui_context::LAYER_BACK, white);
    context->elem_bb[Lui_context::SLOT_PANEL] = Rect {0, 0, context->panel_left_width, global_context.screen_h};

    Padding pad_panel {10, 10};
    s64 x = pad_panel.pad_x;
    s64 y = pad_panel.pad_y - context->scroll_panel.offset;
    s64 w = context->panel_left_width - pad_panel.pad_x*2 - context->width_scrollbar;
    
    auto hsep = [context, x, w, &y, font_inst]() {
        u8 gray[] = {153, 153, 153, 255};
        lui_draw_rect(context, x + 12, y, w - 24, 1, Lui_context::LAYER_MIDDLE, gray);
        y += (s64)std::round(font_inst.newline*1.5f - font_inst.height + 1);
    };

    platform_fmt_draw(Lui_context::SLOT_INITTEXT, x, y, w, &x, &y);
    hsep();

    {s64 x_orig = x;
    platform_fmt_draw(Lui_context::SLOT_LABEL_INPUT, x, y, -1, &x, &y);
    lui_draw_radio(context, x, y, Lui_context::SLOT_LABEL_NUMBERS, &x, &y);
    lui_draw_radio(context, x, y, Lui_context::SLOT_LABEL_FORMULA, &x, &y);
    y += (s64)std::round(font_inst.newline); x = x_orig;}
    
    {y += std::round(font_inst.height - font_inst.ascent);
    s64 slot = create_type_numbers ? Lui_context::ENTRY_NUMBERS : Lui_context::ENTRY_FORMULA;
    lui_draw_entry(context, &context->entries[slot], x, y, w, -1,
        nullptr, &y, nullptr, &context->resizers[Lui_context::RESIZER_CREATE]);
    y += std::round(font_inst.height - font_inst.ascent);}

    {s64 x_orig = x, ha_line;
    lui_draw_entry(context, &context->entries[Lui_context::ENTRY_BASE], x, y, (s64)std::round(font_inst.space*12.f), 1, nullptr, nullptr, &ha_line, nullptr, true);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_LABEL_BASE, x, y, -1, &x, &y);
    y -= ha_line;
    lui_draw_entry(context, &context->entries[Lui_context::ENTRY_BASE], x, y, (s64)std::round(font_inst.space*12.f), 1, &x, nullptr, nullptr);
    y += ha_line; x += 10;
    s64 entry      = create_type_numbers ? Lui_context::ENTRY_BITORDER : Lui_context::ENTRY_VARORDER;
    s64 slot_label = create_type_numbers ? Lui_context::SLOT_LABEL_BITORDER : Lui_context::SLOT_LABEL_VARORDER;
    platform_fmt_draw(slot_label, x, y, -1, &x, &y);
    y -= ha_line;
    lui_draw_entry(context, &context->entries[entry], x, y, (s64)std::round(font_inst.space*30.f), 1, nullptr, &y, nullptr);
    y += (s64)std::round(font_inst.height - font_inst.ascent); x = x_orig;}
    
    {s64 w_line, ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_BUTTON_CREATE, x, y, w, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_CREATE, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    {s64 x_orig = x;
    platform_fmt_draw(Lui_context::SLOT_LABEL_OPERATION, x, y, -1, &x, &y);
    lui_draw_radio(context, x, y, Lui_context::SLOT_LABEL_UNION, &x, &y);
    lui_draw_radio(context, x, y, Lui_context::SLOT_LABEL_INTERSECTION, &x, &y);
    lui_draw_radio(context, x, y, Lui_context::SLOT_LABEL_COMPLEMENT, &x, &y);
    y += (s64)std::round(font_inst.newline); x = x_orig;}

    y += std::round(font_inst.height - font_inst.ascent);

    {s64 x_orig = x, ha_line;
    lui_draw_entry(context, &context->entries[Lui_context::ENTRY_FIRSTNODE], x, y, (s64)std::round(font_inst.space*12.f), 1, nullptr, nullptr, &ha_line, nullptr, true);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_LABEL_FIRSTNODE, x, y, -1, &x, &y);
    y -= ha_line;
    lui_draw_entry(context, &context->entries[Lui_context::ENTRY_FIRSTNODE], x, y, (s64)std::round(font_inst.space*12.f), 1, &x, nullptr, nullptr);
    y += ha_line; x += 10;
    platform_fmt_draw(Lui_context::SLOT_LABEL_SECONDNODE, x, y, -1, &x, &y);
    y -= ha_line;
    lui_draw_entry(context, &context->entries[Lui_context::ENTRY_SECONDNODE], x, y, (s64)std::round(font_inst.space*12.f), 1, nullptr, &y, nullptr);
    y += (s64)std::round(font_inst.height - font_inst.ascent); x = x_orig;}

    {s64 w_line, ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_BUTTON_OP, x, y, w, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_OP, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    {s64 w_line, ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_BUTTON_REMOVEALL, x, y, w, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_REMOVEALL, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    {s64 w_line, ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_BUTTON_HELP, x, y, w, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_HELP, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    if (context->fmt_slots[Text_fmt::SLOT_ERRORINFO].size) {
        platform_fmt_draw(Text_fmt::SLOT_ERRORINFO, x, y, w, nullptr, &y);
        hsep();
    }
    
    {s64 w_line, ha_line, w_label, pad_x = 5;
    lui_draw_button_right(context, Lui_context::SLOT_BUTTON_NEXT, x, y, w, &ha_line, &w_line);
    platform_fmt_draw(Text_fmt::SLOT_CONTEXT_FRAME, 0, 0, -1, &w_label, nullptr, true);
    y += ha_line; w_line -= w_label + pad_x;
    platform_fmt_draw(Text_fmt::SLOT_CONTEXT_FRAME, x+w_line, y, -1, nullptr, nullptr);
    y -= ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_BUTTON_PREV, x, y, w_line-pad_x, nullptr, &w_line);
    y += ha_line; 
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_CONTEXT, x, y, w_line, nullptr, &y);}

    y += std::round(font_inst.newline * 0.5f);
    platform_fmt_draw(Text_fmt::SLOT_CONTEXT, x, y, w, nullptr, &y);

    if (context->bddinfo_active) {
        auto c = &global_context;
        Padding pad {8, 8};
        s64 w = 345, iw, ih;
        platform_fmt_draw(Text_fmt::SLOT_BDDINFO, 0, 0, w-2*pad.pad_x, nullptr, &ih, true, &iw);
        s64 x = context->bddinfo_x + context->bddinfo_pad;
        s64 y = context->bddinfo_y;

        if (x + pad.pad_x*2 + iw > c->width) {
            x -= iw + pad.pad_x*2 + context->bddinfo_pad * 2;
        }
        if (y + pad.pad_y*2 + ih > c->height) {
            y -= ih + pad.pad_y*2;
        }
        x += c->canvas_x; y += c->canvas_y;
        
        platform_fmt_draw(Text_fmt::SLOT_BDDINFO, x+pad.pad_x, y+pad.pad_y, iw, nullptr, nullptr);
        lui_draw_rect(context, x, y, iw + pad.pad_x*2, ih + pad.pad_y*2, Lui_context::LAYER_MIDDLE, white);
    }

    if (context->helptext_active) {
        auto c = &global_context;
        Padding pad {8, 16, 10, 10};
        s64 w = std::min((s64)c->width - pad.mar_x*2, 750ll);

        s64 h;
        platform_fmt_draw(Lui_context::SLOT_HELPTEXT, 0, 0, w-2*pad.pad_x, nullptr, &h, true);
        h = std::min((s64)c->height - pad.mar_y*2, h + pad.pad_y*2);

        s64 x = ((s64)c->width  - w) / 2 + c->canvas_x;
        s64 y = ((s64)c->height - h) / 2 + c->canvas_y;
        lui_draw_button_right(context, Lui_context::SLOT_BUTTON_HELP_CLOSE, x+pad.pad_x, y+pad.pad_y, w-2*pad.pad_x, nullptr, nullptr);
        platform_fmt_draw(Lui_context::SLOT_HELPTEXT, x+pad.pad_x, y+pad.pad_y, w-2*pad.pad_x, nullptr, nullptr);
        lui_draw_rect(context, x, y, w, h, Lui_context::LAYER_BACK, white);
    }
    y += pad_panel.pad_y;

    context->scroll_panel.total_height = y + context->scroll_panel.offset;

    lui_draw_scrollbar(context, &context->scroll_panel);
    
    _platform_frame_draw();
    glXSwapBuffers(platform->display, platform->window_glx);

    if (context->main_loop_active) {
        platform_redraw(0);
    }
}

void linux_get_event_key(Array_dyn<Key>* keys, XKeyEvent e) {
    KeySym keysym;

    // You would think that we could do some dynamic resizing here if the buffer is too
    // small. However, the API does not seem to support it.
    char buffer_[64];
    Array_t<char> buffer {buffer_, sizeof(buffer)};
    buffer.size = XLookupString(&e, buffer.data, buffer.size, &keysym, NULL);
    
    s64 special = Key::INVALID;
    u64 mod = 0;
    u64 shiftctrl = ShiftMask | ControlMask;
    switch (keysym) {
    case XK_Escape:       special = Key::ESCAPE; break;
    case XK_Page_Up:      special = Key::PAGE_U; break;
    case XK_Page_Down:    special = Key::PAGE_D; break;
    case XK_Tab:          special = Key::TAB;    break;
    case XK_F1:           special = Key::F1;     break;
    case XK_F2:           special = Key::F2;     break;
    case XK_F3:           special = Key::F3;     break;
    case XK_F4:           special = Key::F4;     break;
    case XK_F5:           special = Key::F5;     break;
    case XK_F6:           special = Key::F6;     break;
    case XK_F7:           special = Key::F7;     break;
    case XK_F8:           special = Key::F8;     break;
    case XK_F9:           special = Key::F9;     break;
    case XK_F10:          special = Key::F10;    break;
    case XK_F11:          special = Key::F11;    break;
    case XK_F12:          special = Key::F12;    break;
    case XK_Return:       special = Key::RETURN; break;
    case XK_Left:         special = Key::ARROW_L;   mod = e.state & shiftctrl; break;
    case XK_Right:        special = Key::ARROW_R;   mod = e.state & shiftctrl; break;
    case XK_Down:         special = Key::ARROW_D;   mod = e.state & shiftctrl; break;
    case XK_Up:           special = Key::ARROW_U;   mod = e.state & shiftctrl; break;
    case XK_Home:         special = Key::HOME;      mod = e.state & shiftctrl; break;
    case XK_End:          special = Key::END;       mod = e.state & shiftctrl; break;
    case XK_Delete:       special = Key::DELETE;    mod = e.state & shiftctrl; break;
    case XK_BackSpace:    special = Key::BACKSPACE; mod = e.state & shiftctrl; break;
    case XK_ISO_Left_Tab: special = Key::SHIFT_TAB;   mod = ShiftMask;   break;
    case XK_c:            special = Key::C_COPY;      mod = ControlMask; break;
    case XK_v:            special = Key::C_PASTE;     mod = ControlMask; break;
    case XK_x:            special = Key::C_CUT;       mod = ControlMask; break;
    case XK_a:            special = Key::C_SELECTALL; mod = ControlMask; break;
    case XK_q:            special = Key::C_QUIT;      mod = ControlMask; break;
    case XK_s:            special = Key::C_SAVE;      mod = ControlMask; break;
    case XK_z:            special = Key::C_UNDO;      mod = ControlMask; break;
    case XK_Z:            special = Key::C_REDO;      mod = ControlMask | ShiftMask; break;
    }

    // Exclude NumLock (Mod2) and CapsLock (Lock) from the modifier list, as our shortcuts should
    // still work if they are pressed.
    u64 mod_mask = ShiftMask | ControlMask | Mod1Mask | Mod3Mask | Mod4Mask | Mod5Mask;
    if (special != Key::INVALID and (e.state & mod_mask) == mod) {
        u8 flags = 0;
        if (e.state & ShiftMask) flags |= Key::MOD_SHIFT;
        if (e.state & ControlMask) flags |= Key::MOD_CTRL;
        array_push_back(keys, Key::create_special(special, flags));
    } else {
        s64 chunk = sizeof(Key::text);
        for (s64 i = 0; i < buffer.size; i += chunk) {
            s64 end = i + chunk < buffer.size ? i + chunk : buffer.size;
            array_push_back(keys, Key::create_text(array_subarray(buffer, i, end)));
        }
    }
}

void platform_set_cursor(u8 type) {
    if (type == global_platform.cursor_type) return;

    global_platform.cursor_type = type;
    if (type == Cursor_type::TEXT) {
        XDefineCursor(global_platform.display, global_platform.window, global_platform.cursor_text);
    } else if (type == Cursor_type::RESIZE) {
        XDefineCursor(global_platform.display, global_platform.window, global_platform.cursor_resize);
    } else if (type == Cursor_type::NORMAL) {
        XUndefineCursor(global_platform.display, global_platform.window);
    } else {
        assert(false);
    }
}

void linux_set_wm_prop(Display* display, Window window, char const* property, char const* data) {
    Atom prop = XInternAtom(display, property, true);
    assert(prop != None);

    Atom type_string = XInternAtom(display, "STRING", true);
    assert(type_string != None);
    
    XChangeProperty(display, window, prop, type_string, 8, PropModeReplace, (u8*)data, strlen(data));
}

void linux_set_wm_class(Display* display, Window window, int argc, char** argv) {
    char* instance = nullptr;
    for (s64 i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-name") == 0 and i+1 < argc) {
            instance = argv[i+1];
        }
    }
    if (not instance) {
        instance = getenv("RESOURCE_NAME");
    }
    if (not instance) {
        s64 last_sep = 0;
        for (s64 i = 0; argv[0][i]; ++i) {
            if (argv[0][i] == '/') last_sep = i;
        }
        if (argv[0][last_sep] and argv[0][last_sep+1]) {
            instance = &argv[0][last_sep + 1];
        }
    }
    if (not instance) {
        instance = (char*)"obst";
    }

    char* cls = (char*)"obst";
    s64 buf_size = strlen(instance) + strlen(cls) + 2;
    char* buf = (char*)alloca(buf_size);
    assert( snprintf(buf, buf_size, "%s %s", instance, cls) < buf_size );
    
    linux_set_wm_prop(display, window, "WM_CLASS", buf);
}

Array_t<u8> platform_clipboard_get(s64 index) {
    s64 size = *(s64*)&global_platform.clipboard_recv[index];
    index += sizeof(s64);
    return array_subarray(global_platform.clipboard_recv, index, index+size);
}
void platform_clipboard_free(s64 index) {
    auto arr = platform_clipboard_get(index);
    if (arr.end() == global_platform.clipboard_recv.end()) {
        global_platform.clipboard_recv.size = 0;
    }
}
void platform_clipboard_set(u8 type, Array_t<u8> data) {
    assert(type <= Platform_clipboard::COUNT);
    global_platform.clipboard_send[type].size = 0;
    array_append(&global_platform.clipboard_send[type], data);
    auto type_x = type == Platform_clipboard::CONTROL_C ? global_platform.sel_clipboard : global_platform.sel_primary;
    XSetSelectionOwner(global_platform.display, type_x, global_platform.window, CurrentTime);
}
void linux_handle_selection_request(Platform_state* platform, XSelectionRequestEvent* ev) {
    XSelectionEvent ev_out;
    ev_out.type = SelectionNotify;
    ev_out.requestor = ev->requestor;
    ev_out.selection = ev->selection;
    ev_out.target    = ev->target;
    ev_out.time      = ev->time;

    if (ev->target == platform->sel_utf8str or ev->target == platform->sel_string) {
        Atom property = ev->property;
        if (property == None) {
            property = ev->target;
        }

        u8 sel_type = ev->selection == platform->sel_primary ? Platform_clipboard::MIDDLE_BUTTON : Platform_clipboard::CONTROL_C;
        auto buf = platform->clipboard_send[sel_type];
        XChangeProperty(platform->display, ev->requestor, property, ev->target, 8, PropModeReplace, buf.data, buf.size);
        
        ev_out.property = property;
    } else {
        ev_out.property = None;
    }

    XSendEvent(platform->display, ev_out.requestor, false, 0, (XEvent*)&ev_out);
}
    
void linux_handle_selection_response(Platform_state* platform, XSelectionEvent* ev) {
    if (ev->property == None) {
        // Try to fall back to STRING type
        XConvertSelection(platform->display, ev->selection, platform->sel_target, platform->sel_string, platform->window, ev->time);
    } else {
        Atom actual_type;
        int actual_format;
        unsigned long n_items, bytes_after;
        u8* prop;
        XGetWindowProperty(platform->display, platform->window, platform->sel_target, 0, -1, 0,
            AnyPropertyType, &actual_type, &actual_format, &n_items, &bytes_after, &prop);

        if (actual_type == platform->sel_incr) {
            fprintf(stderr, "Warning: Received clipboard of type INCR, which obst does not implement.\n");
            return;
        } else if (actual_type == None or actual_format != 8) {
            return;
        }
        
        s64 index = global_platform.clipboard_recv.size;
        s64 n_items_ = n_items;
        array_append(&global_platform.clipboard_recv, {(u8*)&n_items_, sizeof(s64)});
        array_append(&global_platform.clipboard_recv, {prop, (s64)n_items});

        XFree(prop);
        XDeleteProperty(platform->display, platform->window, platform->sel_target);

        if (ev->selection == platform->sel_primary) {
            Key key1 = Key::create_mouse(Key::LEFT_DOWN, platform->gl_context.pointer_x, platform->gl_context.pointer_y);
            Key key2 = Key::create_mouse(Key::LEFT_UP,   platform->gl_context.pointer_x, platform->gl_context.pointer_y);
            array_append(&global_platform.gl_context.input_queue, {key1, key2});
        }
        
        Key key = Key::create_special(Key::C_PASTE, 0, index);
        array_push_back(&global_platform.gl_context.input_queue, key);
    }
}

bool _platform_fonts_load(Lui_context* context) {
    constexpr char const* font_files_[] = {
        "fonts_stripped/DejaVuSerif.ttf",
        "fonts_stripped/DejaVuSerif-Italic.ttf",
        "fonts_stripped/DejaVuSerif-Bold.ttf",
        "fonts_stripped/DejaVuSans.ttf",
        "fonts_stripped/LICENSE", // Note: The last filename will not be instanciated in font_info
    };
    context->font_files = {(u8**)font_files_, sizeof(font_files_) / sizeof(font_files_[0])};
    
    char const* path = "/proc/self/exe";
    FILE* f = fopen(path, "rb");
    
    if (f == nullptr) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (7)\n", path);
        perror("Error"); exit(1125);
    }

    if (fseek(f, 0, SEEK_END) == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (8)\n", path);
        perror("Error"); exit(1126);
    }

    s64 f_size = ftell(f);
    if (f_size == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (9)\n", path);
        perror("Error"); exit(1127);
    }

    struct Trailer {
        char magic[sizeof(Lui_context::font_magic)];
        s64 data_offset;
    };
    static_assert(sizeof(Lui_context::font_magic) % 8 == 0, "Trailer struct is not packed anymore");

    auto read = [f, path](u8* into, s64 bytes) {
        if (fread(into, 1, bytes, f) < bytes) {
            if (ferror(f)) {
                fprintf(stderr, "Error: Could not open file '%s' for reading (10)\n", path);
                perror("Error"); exit(1129);
            } else {
                fprintf(stderr, "Error: Could not open file '%s' for reading (11)\n", path);
                fprintf(stderr, "Error: Unexpected eof while reading file. (Concurrent modification?)\n");
                exit(1130);
            }
        }
    };
    
    Trailer trailer;

    if (f_size < sizeof(trailer)) {
        fprintf(stderr, "Error: File '%s' is less than %d bytes long\n", path, (int)sizeof(trailer));
        exit(1131);
    }
    if (fseek(f, -sizeof(trailer), SEEK_CUR) == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for seeking (1)\n", path);
        perror("Error"); exit(1128);
    }
    read((u8*)&trailer, sizeof(trailer));

    // @Leak: This is never freed intentionally, the fonts are used for the whole duration
    // of the program.
    context->font_file_data = array_create<Array_t<u8>>(context->font_files.size);
    
    bool result;
    if (memcmp(trailer.magic, &Lui_context::font_magic, sizeof(Lui_context::font_magic)) != 0) {
        // The magic number is not there, so we load the fonts as separate files

        for (s64 i = 0; i < context->font_files.size; ++i) {
            context->font_file_data[i] = array_load_from_file((char*)context->font_files[i]);
        }
        result = false;
    } else {
        // Extract the data from the executable

        if (fseek(f, trailer.data_offset, SEEK_SET) == -1) {
            fprintf(stderr, "Error: Could not open file '%s' for seeking (2)\n", path);
            perror("Error"); exit(1132);
        }

        s64 font_count;
        read((u8*)&font_count, sizeof(font_count));

        if (font_count != context->font_files.size) {
            fprintf(stderr, "Error: While loading fonts from executable, got a mismatch in number of fonts\n");
            perror("Error"); exit(1133);
        }

        Array_t<s64> font_lengths {(s64*)alloca(font_count * sizeof(s64)), font_count};
        read((u8*)font_lengths.data, font_count * sizeof(s64));

        for (s64 i = 0; i < font_count; ++i) {
            context->font_file_data[i] = array_create<u8>(font_lengths[i]);
            read((u8*)context->font_file_data[i].data, context->font_file_data[i].size);
        }
        
        result = true;
    }

    if (fclose(f)) {
        // Something weird is happening here, but hey, we already have our data.
        fprintf(stderr, "Warning: Could not close file '%s'\n", path);
        perror("Warning:");
    }
    
    return result;
}

void _platform_fonts_pack(Lui_context* context) {
    char const* path1 = "/proc/self/exe";
    char const* path2 = "obst_packed";
    
    FILE* f1 = fopen(path1, "rb");

    if (f1 == nullptr) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (12)\n", path1);
        perror("Error"); exit(1201);
    }

    FILE* f2 = fopen(path2, "wb");

    if (f2 == nullptr) {
        fprintf(stderr, "Error: Could not open file '%s' for writing (1)\n", path2);
        perror("Error"); exit(1202);
    }

    int fd = fileno(f2);
    if (fchmod(fd, 0755)) {
        fprintf(stderr, "Warning: Could not mark file '%s' as executable\n", path2);
        perror("Warning");
    }
    
    char buf[4096];
    bool done = false;
    while (not done) {
        s64 read = fread(buf, 1, sizeof(buf), f1);
        if (read < sizeof(buf)) {
            if (ferror(f1)) {
                fprintf(stderr, "Error: Could not open file '%s' for reading (13)\n", path1);
                perror("Error"); exit(1203);
            }
            done = true;
        }
        
        if (fwrite(buf, 1, read, f2) < read) {
            fprintf(stderr, "Error: Could not open file '%s' for writing (2)\n", path2);
            perror("Error"); exit(1204);
        }
    }
    
    s64 f_size = ftell(f2);
    if (f_size == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (14)\n", path2);
        perror("Error"); exit(1205);
    }

    auto write = [f2, path2](u8* data, s64 size) {
        if (fwrite(data, 1, size, f2) < size) {
            fprintf(stderr, "Error: Could not open file '%s' for writing (3)\n", path2);
            perror("Error"); exit(1206);
        }
    };

    write((u8*)&context->font_files.size, sizeof(s64));
    for (Array_t<u8> i: context->font_file_data) {
        write((u8*)&i.size, sizeof(s64));
    }
    for (Array_t<u8> i: context->font_file_data) {
        write((u8*)i.data, i.size);
    }
    write((u8*)Lui_context::font_magic, sizeof(Lui_context::font_magic));
    write((u8*)&f_size, sizeof(s64));

    if (fclose(f2)) {
        fprintf(stderr, "Error: Could not close file '%s'\n", path2);
        perror("Error:"); exit(1207);
    }
    
    if (fclose(f1)) {
        // Something weird is happening here, but hey, we already have our data.
        fprintf(stderr, "Warning: Could not close file '%s'\n", path1);
        perror("Warning:");
    }
}

void _platform_print_help(char* argv0, bool is_packed) {
    printf("Usage:\n  %s\n", argv0);
    if (is_packed) {
        printf("  %s --font-license\n", argv0);
    } else {
        printf("  %s --pack\n", argv0);
    }
    printf("  %s --help\n\n", argv0);

    puts("This is obst, a visualisation of algorithms related to Binary Decision Diagrams, written by Philipp Czerner in 2018. Running the program without any arguments starts the GUI, which is the main part of this application.\n");
    if (is_packed) {
        puts("You are running the packed version of obst, which meas that the font data is included in the binary. To view the license under which the fonts are distributed, run obst with the --font-license option.");
    } else {
        puts("You are running the unpacked version of the binary, which means that it will load fonts from the 'fonts/' subdirectory of the CWD. You can run this programm with the '--pack' option, which will create a 'obst_packed' binary in the CWD that contains the font data.");
    }
}

int main(int argc, char** argv) {
    bool print_help = false;
    bool is_packed = _platform_fonts_load(&global_platform.gl_context);
    
    if (argc > 2) {
        print_help = true;
    } else if (argc == 2) {
        Array_t<u8> arg = {(u8*)argv[1], (s64)strlen(argv[1])};
        if (array_equal_str(arg, "--font-license")) {
            auto arr = global_platform.gl_context.font_file_data;
            fwrite(arr[arr.size-1].data, 1, arr[arr.size-1].size, stdout);
            puts("");
            exit(0);
        } else if (array_equal_str(arg, "--pack")) {
            _platform_fonts_pack(&global_platform.gl_context);
            printf("Packing successful.\n");
            exit(0);
        } else {
            print_help = true;
        }
    }

    if (print_help) {
        _platform_print_help(argv[0], is_packed);
        exit(2);
    }
    
    // Do the OpenGL and X dance. I would recommend everyone to not read this code, if at all
    // possible, to preserve sanity. This should have been a single function call. If you must,
    // refer to the GLX 1.4 specification, the GLX_ARB_create_context extension, and the Xlib
    // specification to understand what all of this does.

    if (setlocale(LC_ALL, "") == nullptr) {
        fprintf(stderr, "Warning: Could not set default locale.\n");
    }
    
    // Create the display, connect to the X server
    Display* display = XOpenDisplay(nullptr);
    if (not display) {
        fprintf(stderr, "Error: could not open display (is the DISPLAY environment variable set correctly?)\n");
        exit(101);
    }
    global_platform.display = display;

    int screen = DefaultScreen(display);
    
    // Check for GLX 1.4 and extensions
    int glx_version[] = {1, 4};
    if (not glXQueryVersion(display, &glx_version[0], &glx_version[1])) {
        fprintf(stderr, "Error: glX version %d.%d not present\n", glx_version[0], glx_version[1]);
        exit(102);
    }

    char* gl_ext_present = (char*)glXQueryExtensionsString(display, screen);
    char* gl_ext_want[] = {(char*)"GLX_ARB_create_context_profile"};
    for (s64 i = 0; i < (s64)(sizeof(gl_ext_want) / sizeof(gl_ext_want[0])); ++i) {
        if (not strstr(gl_ext_present, gl_ext_want[i])) {
            fprintf(stderr, "Error: OpenGL extension %s not present\n", gl_ext_want[i]);
            exit(105);
        }
    }

    int config_size;
    int config_attribs[] = {
        GLX_DOUBLEBUFFER, true,
        GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
        GLX_DEPTH_SIZE, 8,
        None
    };
    GLXFBConfig* config = glXChooseFBConfig(display, screen, config_attribs, &config_size);
    //defer { XFree(config); };
    if (not config or config_size == 0) {
        fprintf(stderr, "Error: did not find a GLXFBConfig with the required attributes\n");
        exit(103);
    }

    // TODO: This seems to be unnecessary? Try to test on some older systems.
    // Create old OpenGL context, to be able to get the function pointer for creating the new one.
    //GLXContext context_old = glXCreateNewContext(display, *config, GLX_RGBA_TYPE, 0, true);
    //if (not context_old) {
    //    fprintf(stderr, "Error: failed to initialise old GLXContext\n");
    //    exit(104);
    //}

    glXCreateContextAttribsARB_t glXCreateContextAttribsARB
        = (glXCreateContextAttribsARB_t)glXGetProcAddress((u8*)"glXCreateContextAttribsARB");
    assert(glXCreateContextAttribsARB); // We already checked for the presence of the extension

    //glXDestroyContext(display, context_old);

    // Create an OpenGL 3.2 context
    int context_attribs[] = {
        GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
        GLX_CONTEXT_MINOR_VERSION_ARB, 2,
        GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        GLX_CONTEXT_PROFILE_MASK_ARB, GLX_CONTEXT_CORE_PROFILE_BIT_ARB,
        None
    };
    GLXContext context = glXCreateContextAttribsARB(display, *config, 0, true, context_attribs);

    // Create the window
    XVisualInfo* visual = glXGetVisualFromFBConfig(display, *config);
    assert(visual);
    //defer { XFree(visual); };

    XSetWindowAttributes window_attrs = {};
    window_attrs.colormap = XCreateColormap(display, DefaultRootWindow(display), visual->visual, AllocNone); // Apparently you need the colormap, else XCreateWindow gives a BadMatch error. No worries, this fact features prominently in the documentation and it was no bother at all.
    window_attrs.event_mask = ExposureMask | KeyPressMask | KeyReleaseMask | ButtonPressMask
        | ButtonReleaseMask | StructureNotifyMask | PointerMotionMask | FocusChangeMask;

    Window window = XCreateWindow(display, DefaultRootWindow(display), 0, 0, 1300, 800, 0,
        visual->depth, InputOutput, visual->visual, CWColormap | CWEventMask, &window_attrs); // We pass a type of InputOutput explicitly, as visual->c_class is an illegal value for some reason. A good reason, I hope.
    global_platform.window = window;

    // Set up xrandr
    int xrandr_event, xrandr_error;
    if (XRRQueryExtension(display, &xrandr_event, &xrandr_error)) {
        XRRSelectInput(display, window, RRScreenChangeNotifyMask);

        XRRScreenConfiguration* config = XRRGetScreenInfo(display, window);
        global_platform.rate = XRRConfigCurrentRate(config);
        XRRFreeScreenConfigInfo(config);
    } else {
        fprintf(stderr, "Warning: Xrandr extension not present on X server, assuming refresh rate of 60 Hz.\n");
        global_platform.rate = 60;
    }

    // Initialise window properties
    linux_set_wm_prop(display, window, "WM_NAME", "obst - Binary Decision Diagrams");
    linux_set_wm_prop(display, window, "WM_ICON_NAME", "obst");
    linux_set_wm_class(display, window, argc, argv);

    // Set WM_PROTOCOLS
    Atom wm_protocols = XInternAtom(display, "WM_PROTOCOLS", true);
    Atom wm_delete_window = XInternAtom(display, "WM_DELETE_WINDOW", true);
    Atom type_atom = XInternAtom(display, "ATOM", true);
    assert(wm_protocols != None and wm_delete_window != None and type_atom != None);
    XChangeProperty(display, window, wm_protocols, type_atom, 32, PropModeReplace, (u8*)&wm_delete_window, 1);

    // Initialise cursor
    global_platform.cursor_text   = XCreateFontCursor(display, 152);
    global_platform.cursor_resize = XCreateFontCursor(display, 116);

    // Query some atom we will later need for the clipboard
    Atom sel_primary   = XInternAtom(display, "PRIMARY", true);
    Atom sel_clipboard = XInternAtom(display, "CLIPBOARD", true);
    Atom sel_utf8str   = XInternAtom(display, "UTF8_STRING", true);
    Atom sel_string    = XInternAtom(display, "STRING", true);
    Atom sel_target    = XInternAtom(display, "SELECTION_TARGET", false); // This one is arbitrary
    Atom sel_incr      = XInternAtom(display, "INCR", true);
    global_platform.sel_primary   = sel_primary;
    global_platform.sel_clipboard = sel_clipboard;
    global_platform.sel_utf8str   = sel_utf8str;
    global_platform.sel_string    = sel_string;
    global_platform.sel_target    = sel_target;
    global_platform.sel_incr      = sel_incr;
    
    // Map the context to the window
    GLXWindow window_glx = glXCreateWindow(display, *config, window, nullptr);
    global_platform.window_glx = window_glx;
    glXMakeContextCurrent(display, window_glx, window_glx, context); // This returns a bool, but I cannot find what it means in the spec, so just ignore it. The greatness continues.

    // Do application-specific initialisation
    _platform_init(&global_platform);

    // Show the window
    XMapWindow(display, window);

    int x_fd = ConnectionNumber(display);

    double dbg = platform_now();
    while (true) {
        bool redraw = false;
        if (XPending(display) <= 0) {
            // No events left to process, now we redraw or wait
            
            double wait = global_platform.redraw_next - platform_now();
            if (wait == INFINITY) {
                // Wait for next event
            } else if (wait * (double)global_platform.rate > 0.5f) {
                fd_set fds;
                FD_ZERO(&fds);
                FD_SET(x_fd, &fds);
                
                timeval t;
                t.tv_sec = (long)wait;
                t.tv_usec = (long)((wait - (double)t.tv_sec)*1e6);
                
                int num = select(x_fd+1, &fds, nullptr, nullptr, &t);
                if (num == -1) {
                    fprintf(stderr, "Error: while executing select()\n");
                    perror("Error");
                    exit(105);
                } else if (num == 0) {
                    // Got timeout, redraw
                    redraw = true;
                } else {
                    // Check whether there is really an event there, or this is just one of select's
                    // classic wakeup pranks.
                    if (XPending(display) <= 0) continue;
                    
                    // An event arrived, process.
                }
            } else {
                // Next frame is imminent, draw
                redraw = true;
            }
        }
        
        if (redraw) {
            global_platform.redraw_last = global_platform.redraw_next;
            global_platform.redraw_next = INFINITY;
            _platform_render(&global_platform);
            continue;
        }
        
        XEvent event;
        XNextEvent(display, &event);

        switch (event.type) {
        case Expose:
            platform_redraw(0);
            break;
        case KeyPress: {
            auto* iq = &global_platform.gl_context.input_queue;
            linux_get_event_key(iq, event.xkey);
            if (iq->size and (*iq)[iq->size-1].type == Key::SPECIAL and (*iq)[iq->size-1].special == Key::C_PASTE) {
                // Query the contents of the clipboard, we need to wait for them to arrive
                XConvertSelection(display, sel_clipboard, sel_utf8str, sel_target, window, event.xkey.time);
                --iq->size;
            } else {
                platform_redraw(0);
            }
        } break;
        case ButtonPress:
            global_platform.gl_context.pointer_x = event.xmotion.x;
            global_platform.gl_context.pointer_y = event.xmotion.y;
            if (event.xbutton.button == Button1) {
                Key key = Key::create_mouse(Key::LEFT_DOWN, event.xbutton.x, event.xbutton.y);
                array_push_back(&global_platform.gl_context.input_queue, key);
                platform_redraw(0);
            } else if (event.xbutton.button == Button4) {
                Key key = Key::create_mouse(Key::SCROLL_UP, event.xbutton.x, event.xbutton.y);
                array_push_back(&global_platform.gl_context.input_queue, key);
                platform_redraw(0);
            } else if (event.xbutton.button == Button5) {
                Key key = Key::create_mouse(Key::SCROLL_DOWN, event.xbutton.x, event.xbutton.y);
                array_push_back(&global_platform.gl_context.input_queue, key);
                platform_redraw(0);
            } else if (event.xbutton.button == Button2) {
                XConvertSelection(display, sel_primary, sel_utf8str, sel_target, window, event.xbutton.time);
            }
            break;
        case ButtonRelease:
            global_platform.gl_context.pointer_x = event.xmotion.x;
            global_platform.gl_context.pointer_y = event.xmotion.y;
            if (event.xbutton.button == Button1) {
                Key key = Key::create_mouse(Key::LEFT_UP, event.xbutton.x, event.xbutton.y);
                array_push_back(&global_platform.gl_context.input_queue, key);
                platform_redraw(0);
            }
            break;
        case MotionNotify: {
            global_platform.gl_context.pointer_x = event.xmotion.x;
            global_platform.gl_context.pointer_y = event.xmotion.y;
            Key key = Key::create_mouse(Key::MOTION, event.xmotion.x, event.xmotion.y);
            array_push_back(&global_platform.gl_context.input_queue, key);
            platform_redraw(0);
        } break;
            
        case SelectionNotify:
            linux_handle_selection_response(&global_platform, &event.xselection);
            platform_redraw(0);
            break;
        case SelectionRequest:
            linux_handle_selection_request(&global_platform, &event.xselectionrequest);
            break;

        case FocusIn: {
            Key key = Key::create_general(Key::FOCUS_IN);
            array_push_back(&global_platform.gl_context.input_queue, key);
            platform_redraw(0);
        } break;
        case FocusOut: {
            Key key = Key::create_general(Key::FOCUS_OUT);
            array_push_back(&global_platform.gl_context.input_queue, key);
            platform_redraw(0);
        } break;
            
        case MappingNotify:
            if (event.xmapping.request == MappingModifier or event.xmapping.request == MappingKeyboard) {
                XRefreshKeyboardMapping(&event.xmapping);
            }
            break;
        case ClientMessage:
            if (event.xclient.message_type == wm_protocols and event.xclient.data.l[0] - wm_delete_window == 0) {
                array_push_back(&global_platform.gl_context.input_queue, Key::create_special(Key::C_QUIT, 0));
                platform_redraw(0);
            }
            break;
        case ConfigureNotify:
            _platform_handle_resize(event.xconfigure.width, event.xconfigure.height);
            break;
        default:
            break;
        }

        if (event.type == xrandr_event + RRScreenChangeNotify) {
            assert(XRRUpdateConfiguration(&event));
            XRRScreenConfiguration* config = XRRGetScreenInfo(display, window);
            global_platform.rate = XRRConfigCurrentRate(config);
            XRRFreeScreenConfigInfo(config);
        }
    }

    // Memory freed by OS
}


