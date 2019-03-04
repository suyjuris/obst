
// Written by Philipp Czerner, 2018
// See the file obst.cpp for license information.

// This file contains platform-specific code for Linux, responsible for initialising a window and 3D
// rendering context, as well as drawing a UI.

#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <GL/glx.h>
#include <time.h>
#include <locale.h>

#ifndef X_HAVE_UTF8_STRING
#error "There is no Xutf8LookupString! You are on an old version of X."
#endif

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
    int exit_code;

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

namespace Text_fmt {

enum Flags: u64 {
    PARAGRAPH = 1, // Indicates a paragraph break at the end of the item
    NEWLINE = 2,
    NOSPACE = 4, // Do not leave a space after the word. Internal flag for lui_text_draw
    HEADER = 8, // Corresponds to <h4>, draw text as title
    BOLD = 16,
    ITALICS = 32,
    SMALL = 64,
    SANS = 128,
    COMPACT = 256,
    GROUP_SPACING = PARAGRAPH | NEWLINE | NOSPACE,
};
enum Slots: s64 {
    SLOT_CONTEXT, SLOT_BDDINFO, SLOT_HELPTEXT, SLOT_PLATFORM_FIRST
};

};

struct Text_box {
    float x0 = 0.f, y0 = 0.f, x1 = 0.f, y1 = 0.f;
    float s0 = 0.f, t0 = 0.f, s1 = 0.f, t1 = 0.f;
    float advance = 0.f;
    u8 font;
    u32 flags = 0; // Same as the spacing flags in Text_fmt::Flags
    
    static_assert(Text_fmt::GROUP_SPACING >> 32 == 0, "32-bit not sufficient or Text_box flags");
}; //@Cleanup: Move
struct Text_box_lookup {
    u64 hash = 0; s64 index = -1;
};

struct Text_entry {
    Array_dyn<u8> text;
    s64 cursor, cursor_row, cursor_col;
    float offset_row, offset_col;
    s64 slot;
};

struct Rect {
    s64 x, y, w, h;
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
        FONT_LUI_NORMAL, FONT_LUI_ITALIC, FONT_LUI_BOLD, FONT_LUI_HEADER, FONT_LUI_SMALL, FONT_LUI_SANS, FONT_BDD_LABEL,
        FONT_COUNT
    };

    Array_t<stbtt_fontinfo> font_info;
    Array_t<Font_instance> fonts;

    // Data for generating the texture containing the text (the text preparation)
    Array_t<u8> prep_image; // The current content of the font texture
    s64 prep_size = 0; // Size of the font texture
    s64 prep_x = 0, prep_y = 0; // The next rectangle begins here (+1 padding)
    s64 prep_y_incr = 0; // How far to move y when moving into the next line
    Array_dyn<int> prep_glyph_buf; // Temporary storage to hold the glyphs
    Array_dyn<u8>  prep_image_buf; // Temporary storage to hold pixel data
    bool prep_dirty = false; // Whether we need to re-send the texture to the GPU
    Array_dyn<Text_box> prep_cache; // Holds a copy of everything we have already rendered
    Array_t<Text_box_lookup> prep_cache_lookup; // Hash table for the cache

    // Additional slots for text belonging to the UI
    enum Fmt_slots_lui: s64 {
        SLOT_INITTEXT = Text_fmt::SLOT_PLATFORM_FIRST, SLOT_BUTTON_DESC_CREATE, SLOT_BUTTON_DESC_OP,
        SLOT_BUTTON_DESC_REMOVEALL, SLOT_BUTTON_DESC_HELP, SLOT_BUTTON_DESC_CONTEXT, SLOT_LABEL_BASE,
        SLOT_LABEL_BITORDER, SLOT_LABEL_FIRSTNODE, SLOT_LABEL_SECONDNODE, SLOT_LABEL_FRAME,
        SLOT_ENTRY_NUMBERS, SLOT_ENTRY_BASE, SLOT_ENTRY_BITORDER, SLOT_LABEL_CREATE,
        SLOT_LABEL_UNION, SLOT_LABEL_INTERSECTION, SLOT_LABEL_COMPLEMENT, SLOT_ENTRY_FIRSTNODE,
        SLOT_ENTRY_SECONDNODE, SLOT_LABEL_OP_U, SLOT_LABEL_OP_I, SLOT_LABEL_OP_C,
        SLOT_LABEL_OPERATION, SLOT_LABEL_REMOVEALL, SLOT_LABEL_HELP, SLOT_LABEL_PREV,
        SLOT_LABEL_NEXT,
        SLOT_COUNT
    };
    
    // Data for formatted text display
    u64 fmt_flags; // Current flags
    Array_dyn<Text_box> fmt_boxes; // Set of boxes we are currently generating
    Array_dyn<Array_dyn<Text_box>> fmt_slots; // Stored sets of boxes

    enum Element_flags: u8 {
        DRAW_FOCUSED = 1,
        DRAW_ACTIVE = 2,
        DRAW_PRESSED = 4,
        DRAW_DISABLED = 8,
        DRAW_BUTTON = 16,
        DRAW_ENTRY = 32,
        DRAW_RADIO = 64,
        DRAW_COMPACT = 128,
    };

    // Buffer
    Array_dyn<u8> lui_buffer;

    // State of the ui elements
    Text_entry entry_numbers, entry_base, entry_bitorder, entry_firstnode, entry_secondnode;
    Array_t<u64> elem_flags;
    Array_t<Rect> elem_bb;
    s64 elem_focused = 0;
    s64 elem_tabindex = 0;
    s64 panel_left_width = 475; //@Cleanup: Make this DPI aware

    // Input state
    s64 pointer_x, pointer_y;
    Array_dyn<Key> input_queue;
};

struct Platform_state {
    bool is_active = true;

    Display* display = nullptr;
    Window window;
    GLXWindow window_glx;

    bool cursor_is_text = false;
    Cursor cursor_text;

    Lui_context gl_context; //@Cleanup: Rename to lui_context
};
Platform_state global_platform;

void platform_ui_error_report(Array_t<u8> msg) {}
void platform_ui_error_clear() {}
int platform_text_prepare(int size, int w, float* offsets) {return 0;}
Array_t<u8> platform_ui_get_value(u8 elem) {return {};}
void platform_ui_bddinfo_hide() {}
void platform_ui_bddinfo_show(float x, float y, Array_t<u8> text) {}
void platform_ui_context_set(Array_t<u8> text, int frame, int frame_max) {}
void platform_mouse_position(float* out_x, float* out_y) {}
void platform_ui_button_help () {}
void platform_operations_enable(u32 bdd) {}

void platform_main_loop_active(bool is_active) {
    global_platform.is_active = is_active;
}

double platform_now() {
    timespec t;
    clock_gettime(CLOCK_MONOTONIC_RAW, &t);
    return (double)t.tv_sec + (double)t.tv_nsec * 1e-9;
}

void _platform_handle_resize(s64 width, s64 height) {
    global_context.screen_w = width;
    global_context.screen_h = height;
    
    global_context.width = std::min(global_context.screen_w - global_platform.gl_context.panel_left_width, 800ll);
    global_context.height = global_context.screen_h;
    global_context.canvas_x = global_platform.gl_context.panel_left_width;
    global_context.canvas_y = 0;

    glViewport(0.0, 0.0, global_context.screen_w, global_context.screen_h);
    
    application_handle_resize();
}

void test_init();

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
        "precision mediump float;\n"
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
        "precision mediump float;\n"
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

s64 _decode_utf8(Array_t<u8> buf, u32* c_out = nullptr) {
    u32 c = buf[0];
    s64 c_bytes = c&128 ? c&64 ? c&32 ? c&16 ? 4 : 3 : 2 : -1 : 1;
    if (c_bytes == 1) {
        // nothing
    } else if (c_bytes == 2) {
        c = (buf[0]&0x1f) << 6 | (buf[1]&0x3f);
    } else if (c_bytes == 3) {
        c = (buf[0]&0xf) << 12 | (buf[1]&0x3f) << 6 | (buf[2]&0x3f);
    } else if (c_bytes == 4) {
        c = (buf[0]&0x7) << 18 | (buf[1]&0x3f) << 12 | (buf[2]&0x3f) << 6 | (buf[3]&0x3f);
    } else {
        assert(false);
    }
    if (c_out) *c_out = c;
    return c_bytes;
}

void lui_text_prepare_word(Lui_context* context, u8 font, Array_t<u8> word, Text_box* box, float letter_fac=1.f) {
    assert(box);

    // Lookup in hash table
    u64 hash = 14695981039346656037ull ^ font ^ (word.size << 8) ^ (*(u64*)&letter_fac << 32);
    for (u8 c: word) {
        hash = hash * 1099511628211ull ^ c;
    }
    s64 slot_i = hash % context->prep_cache_lookup.size;
    while (true) {
        auto slot = context->prep_cache_lookup[slot_i];
        if (slot.index == -1) break;
        if (slot.hash == hash) {
            *box = context->prep_cache[slot.index];
            return;
        }
        slot_i = (slot_i + 1) % context->prep_cache_lookup.size;
    }
    // Note that slot_i now points at the next empty slot

    Array_dyn<int> glyphs = context->prep_glyph_buf;
    defer { context->prep_glyph_buf = glyphs; };
    glyphs.size = 0;
    
    float f = context->fonts[font].scale;
    stbtt_fontinfo* fontinfo = &context->font_info[context->fonts[font].info_index];
    
    for (s64 i = 0; i < word.size;) {
        // Decode utf-8
        u32 c;
        s64 c_bytes = _decode_utf8(array_subarray(word, i, word.size), &c);
        
        s32 glyph = stbtt_FindGlyphIndex(fontinfo, c);
        if (glyph) array_push_back(&glyphs, glyph);
        
        i += c_bytes;
    }

    s64 x = context->prep_x;
    s64 y = context->prep_y;
    s64 y_incr = context->prep_y_incr;
    
    Array_dyn<u8> buf = context->prep_image_buf;
    defer { context->prep_image_buf = buf; };

    float shift = 0.f;
    bool draw = false;
    s64 x_orig = x;
    s64 y_orig = y;
    s64 x_init_off = 0;
    s64 y_incr_new = 0;
    s64 size = context->prep_size;
    
    for (s64 k = 0; k < glyphs.size; ++k) {
        int ix0, iy0, ix1, iy1;
        stbtt_GetGlyphBitmapBoxSubpixel(fontinfo, glyphs[k], f, f, shift, 0.f, &ix0, &iy0, &ix1, &iy1);

        s64 w = ix1 - ix0;
        s64 h = iy1 - iy0;

        int adv, lsb;
        stbtt_GetGlyphHMetrics(fontinfo, glyphs[k], &adv, &lsb);

        s64 x0 = (s64)((float)x+shift) + ix0; //@Cleanup x + ix0
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

            for (s64 row = 0; row < h; ++row) {
                for (s64 col = 0; col < w; ++col) {
                    u8* p = &context->prep_image[(x0 + col) + (y0 + row) * size];
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
    
    context->prep_x = x;
    context->prep_y = y;
    context->prep_y_incr = y_incr;
    context->prep_dirty = true;

    // Insert element into hashtable
    if (context->prep_cache.size*4 > context->prep_cache_lookup.size*3) {
        fprintf(stderr, "Error: Text_box cache size limit exceeded.\n");
        exit(201);
    }
    context->prep_cache_lookup[slot_i] = {hash, context->prep_cache.size};
    array_push_back(&context->prep_cache, *box);
}

void lui_text_draw(Lui_context* context, Array_t<Text_box> boxes, s64 x_, s64 y_, s64 w_, u8* fill, s64* x_out, s64* y_out, bool only_measure=false) {
    assert(context);

    if (not boxes.size) return;
    
    float x = (float)x_, y = (float)y_, w = (float)w_;
    float orig_x = x, orig_y = y;
    if (w_ == -1) w = INFINITY;

    u8 black[] = {0, 0, 0, 255};
    if (not fill) fill = black;

    y = std::round(y + context->fonts[boxes[0].font].ascent);

    for (Text_box box: boxes) {
        auto font_inst = context->fonts[box.font];
        
        if (x > orig_x and x + box.x1 > w) {
            x = orig_x;
            y = std::round(y + font_inst.newline);
        }

        if (not only_measure) {
            array_append(&context->buf_uitext_pos, {
                x+box.x0, y+box.y0, x+box.x1, y+box.y0, x+box.x1, y+box.y1,
                x+box.x0, y+box.y0, x+box.x1, y+box.y1, x+box.x0, y+box.y1
            });
            array_append(&context->buf_uitext_tpos, {
                box.s0, box.t0, box.s1, box.t0, box.s1, box.t1, box.s0, box.t0, box.s1, box.t1, box.s0, box.t1
            });
            
            for (s64 j = 0; j < 6; ++j) {
                array_append(&context->buf_uitext_fill, {fill, 4});
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
        } else if (box.flags & Text_fmt::NEWLINE) {
            x = orig_x;
            y = std::round(y + font_inst.newline);
        }
    }

    auto font_inst = context->fonts[boxes[boxes.size-1].font];
    y -= font_inst.ascent;
    
    if (x_out) *x_out = (s64)std::round(x);
    if (y_out) *y_out = (s64)std::round(y);
}

struct Padding {
    s64 pad_x, pad_y, mar_x, mar_y;
};

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
    if (w < h) w = h;
    
    float x1 = (float)bb.x;
    float x4 = (float)bb.x + w + 2.f*mar_x;
    float x2 = x1 + mar_y + h/2.f;
    float x3 = x4 - mar_y - h/2.f;
    float y1 = (float)bb.y;
    float y2 = (float)bb.y + h + 2.f*mar_y;

    
    array_append(&context->buf_uibutton_pos, {
        x1, y2, x1, y2, x1, y1, x2, y2, x2, y1, x3, y2, x3, y1, x4, y2, x4, y1, x4, y1
    });
    float sx = h * 0.5f + mar_x;
    float sy = h * 0.5f + mar_y;
    array_append(&context->buf_uibutton_x, {
        -sx, sy, -sx, sy, -sx, -sy, 0.f, sy, 0.f, -sy, 0.f, sy, 0.f, -sy, sx, sy, sx, -sy, sx, -sy
    });

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
    float size = inwards ? h * 0.5f - 0.15f : h * 0.5f;
    float inner = ~flags & Lui_context::DRAW_RADIO ? 1.f :
        ~flags & Lui_context::DRAW_PRESSED ? 1.f :
        flags & Lui_context::DRAW_DISABLED ? 0.7f
        : 0.2f;
    float circle = flags & Lui_context::DRAW_RADIO ? 2.f : 8.f;
    
    for (s64 i = 0; i < 10; ++i) {
        array_push_back(&context->buf_uibutton_size, size);
        array_append(&context->buf_uibutton_border, {border, 3});
        array_append(&context->buf_uibutton_color, {color, 3});
        array_append(&context->buf_uibutton_grad, {grad, 4});
        array_append(&context->buf_uibutton_circle, {circle, inner});
    }
}

void lui_draw_entry_text(Lui_context* context, Text_entry entry, Rect text_bb, u8 font, u8* fill) {
    auto font_inst = context->fonts[font];
    
    float tx = (float)text_bb.x, ty = (float)text_bb.y, tw = (float)text_bb.w, th = (float)text_bb.h;
    float y = std::round(ty + font_inst.ascent);

    s64 c_i = 0;
    
    for (s64 row = 0; row < (s64)std::floor(entry.offset_row); ++row) {
        while (c_i < entry.text.size and entry.text[c_i++] != '\n');
    }
    y -= (entry.offset_row - std::floor(entry.offset_row)) * font_inst.newline;
    if (c_i == entry.text.size) return;

    while (true) {
        if (c_i >= entry.text.size) break;
        if (y - font_inst.ascent >= ty + th) break;

        float x = tx - entry.offset_col;
        while (true) {
            if (c_i == entry.cursor) {
                lui_draw_rect(context, x-1, y-font_inst.ascent+font_inst.height*0.1f, 1, font_inst.height*0.8f, Lui_context::LAYER_FRONT, fill);
            }
            
            if (c_i >= entry.text.size) break;
            if (entry.text[c_i] == '\n') break;
            // Not entirely accurate, due to left-side-bearing, but should not matter
            if (x >= tx + tw) {
                while (c_i < entry.text.size and entry.text[++c_i] != '\n');
                break;
            }

            s64 c_len = _decode_utf8(array_subarray(entry.text, c_i, entry.text.size));

            Text_box box;
            lui_text_prepare_word(context, font, array_subarray(entry.text, c_i, c_i+c_len), &box);
            box.x0 += x; box.x1 += x; box.y0 += y; box.y1 += y;

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
        
        y = std::round(y + font_inst.newline);
        c_i++;
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
void platform_fmt_text(u64 flags, Array_t<u8> text) {
    platform_fmt_begin(flags);
    flags = global_platform.gl_context.fmt_flags;

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
    } else {
        font = Lui_context::FONT_LUI_NORMAL;
    }

    float letter_fac = flags & Text_fmt::COMPACT ? 0.95f : 1.f;
    
    s64 last = 0;
    for (s64 i = 0; i <= text.size; ++i) {
        // Digits are liable to change, so we render them individually
        bool isdigit = last < text.size and ('0' <= text[last] and text[last] <= '9');
        if (i < text.size and text[i] != ' ' and text[i] != '\n' and not isdigit)  continue;
        
        if (last == i) continue;

        Text_box box;
        lui_text_prepare_word(&global_platform.gl_context, font, array_subarray(text, last, i), &box, letter_fac);
        if (isdigit) box.flags |= Text_fmt::NOSPACE;
        
        array_push_back(&global_platform.gl_context.fmt_boxes, box);
        last = i + not isdigit;
    }
    
    platform_fmt_end(flags);
}
void platform_fmt_text(u64 flags, char const* s) {
    platform_fmt_text(flags, {(u8*)s, (s64)strlen(s)});
}
void platform_fmt_store(s64 slot) {
    assert(0 <= slot);

    Lui_context* context = &global_platform.gl_context;
    
    context->fmt_slots[slot].size = 0;
    array_append(&context->fmt_slots[slot], context->fmt_boxes);
    context->fmt_boxes.size = 0;
}
void platform_fmt_store_simple(u64 flags, char const* str, s64 slot) {
    platform_fmt_init();
    platform_fmt_text(flags, str);
    platform_fmt_store(slot);
}
void platform_fmt_store_simple(u64 flags, Array_t<u8> str, s64 slot) {
    platform_fmt_init();
    platform_fmt_text(flags, str);
    platform_fmt_store(slot);
}
void platform_fmt_draw(s64 slot, s64 x, s64 y, s64 w, s64* x_out, s64* y_out, bool only_measure=false) {
    Lui_context* context = &global_platform.gl_context;
    u8 black[] = {0, 0, 0, 255};
    u8 gray[] = {120, 120, 120, 255};
    u8* fill = context->elem_flags[slot] & Lui_context::DRAW_DISABLED ? gray : black;
    lui_text_draw(context, context->fmt_slots[slot], x, y, w, fill, x_out, y_out, only_measure);
}

void lui_draw_button_right(Lui_context* context, s64 slot, s64 x, s64 y, s64 w, s64* ha_out, s64* w_out) {
    assert(context and x and y and w);

    s64 text_w = 0, text_h = 0;
    u8 font = Lui_context::FONT_LUI_SANS;
    
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

void lui_draw_entry(Lui_context* context, Text_entry entry, s64 x, s64 y, s64 w, s64 rows, s64* x_out, s64* y_out, s64* ha_out, bool only_measure=false) {
    u8 font = Lui_context::FONT_LUI_SANS;
    auto font_inst = context->fonts[font];
    Padding pad {7, 5, 3, 3};

    Rect bb {x, y, w, -1};
    bb.h = rows * (s64)std::round(font_inst.newline) + pad.mar_y*2 + pad.pad_y*2;
    context->elem_bb[entry.slot] = bb;

    context->elem_flags[entry.slot] |= Lui_context::DRAW_ENTRY;
    u64 flags = context->elem_flags[entry.slot];
    
    Rect text_bb;
    lui_draw_buttonlike(context, bb, pad, flags, &text_bb, only_measure);

    if (not only_measure) {
        u8 gray1[] = { 20,  20,  20, 255};
        u8 gray2[] = {140, 140, 140, 255};
        u8* fill = flags & Lui_context::DRAW_DISABLED ? gray2 : gray1;
        lui_draw_entry_text(context, entry, text_bb, font, fill);
    }
    
    if (x_out) *x_out = bb.x + bb.w;
    if (y_out) *y_out = bb.y + bb.h;
    if (ha_out) *ha_out = text_bb.y - y + font_inst.ascent - context->fonts[Lui_context::FONT_LUI_NORMAL].ascent;
}

void lui_draw_radio(Lui_context* context, s64 x, s64 y, u64 slot, s64* x_out, s64* y_out) {
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
    char* font_files[] = {
        "DejaVuSerif.ttf",
        "DejaVuSerif-Italic.ttf",
        "DejaVuSerif-Bold.ttf",
        "DejaVuSans.ttf",
        "DejaVuSans.ttf"
    };
    context->font_info = array_create<stbtt_fontinfo>(sizeof(font_files) / sizeof(font_files[0]));

    for (s64 i = 0; i < context->font_info.size; ++i) {
        // @Leak: We do not store a pointer to the font data directly. However, this data is freed
        // precisely when we exit anyway.
        Array_t<u8> font_data = array_load_from_file(font_files[i]);
        int code = stbtt_InitFont(&context->font_info[i], font_data.data, 0);
        if (code == 0) {
            fprintf(stderr, "Error: Could not parse font data in file %s\n", font_files[i]);
            exit(101);
        }
    }

    context->fonts = array_create<Lui_context::Font_instance>(Lui_context::FONT_COUNT);
    auto set_font = [context](s64 font_style, s64 index, float size) {
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
    };
    set_font(Lui_context::FONT_LUI_NORMAL, 0, 20);
    set_font(Lui_context::FONT_LUI_ITALIC, 1, 20);
    set_font(Lui_context::FONT_LUI_BOLD, 2, 20);
    set_font(Lui_context::FONT_LUI_HEADER, 2, 26);
    set_font(Lui_context::FONT_LUI_SMALL, 0, 15);
    set_font(Lui_context::FONT_LUI_SANS, 4, 16.7);
    set_font(Lui_context::FONT_BDD_LABEL, 3, 20);

    // Initialise font preparation
    context->prep_size = 512;
    context->prep_image = array_create<u8>(context->prep_size * context->prep_size);
    context->prep_cache_lookup = array_create<Text_box_lookup>(4096);
    memset(context->prep_cache_lookup.data, -1, context->prep_cache_lookup.size * sizeof(Text_box_lookup));

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

    platform_fmt_store_simple(Text_fmt::PARAGRAPH, "Adds the BDD to the graph.", Lui_context::SLOT_BUTTON_DESC_CREATE);
    platform_fmt_store_simple(Text_fmt::PARAGRAPH, "Applies the operation.", Lui_context::SLOT_BUTTON_DESC_OP);
    platform_fmt_store_simple(Text_fmt::PARAGRAPH, "Reset the application, delete all nodes.", Lui_context::SLOT_BUTTON_DESC_REMOVEALL);
    platform_fmt_store_simple(Text_fmt::PARAGRAPH, "Display usage instructions.", Lui_context::SLOT_BUTTON_DESC_HELP);

    platform_fmt_store_simple(0, "Base:", Lui_context::SLOT_LABEL_BASE);
    platform_fmt_store_simple(0, "Bit order:", Lui_context::SLOT_LABEL_BITORDER);
    platform_fmt_store_simple(Text_fmt::COMPACT, "Operation:", Lui_context::SLOT_LABEL_OPERATION);
    platform_fmt_store_simple(Text_fmt::COMPACT, "Union", Lui_context::SLOT_LABEL_UNION);
    platform_fmt_store_simple(Text_fmt::COMPACT, "Intersection", Lui_context::SLOT_LABEL_INTERSECTION);
    platform_fmt_store_simple(Text_fmt::COMPACT, "Complement", Lui_context::SLOT_LABEL_COMPLEMENT);
    platform_fmt_store_simple(0, "First node:", Lui_context::SLOT_LABEL_FIRSTNODE);
    platform_fmt_store_simple(0, "Second node:", Lui_context::SLOT_LABEL_SECONDNODE);

    u64 button_flag = Text_fmt::SANS | Text_fmt::COMPACT | Text_fmt::NOSPACE;
    platform_fmt_store_simple(button_flag, "Create and add", Lui_context::SLOT_LABEL_CREATE);
    platform_fmt_store_simple(button_flag, "Calculate union", Lui_context::SLOT_LABEL_OP_U);
    platform_fmt_store_simple(button_flag, "Calculate intersection", Lui_context::SLOT_LABEL_OP_I);
    platform_fmt_store_simple(button_flag, "Calculate complement", Lui_context::SLOT_LABEL_OP_C);
    platform_fmt_store_simple(button_flag, "Remove all", Lui_context::SLOT_LABEL_REMOVEALL);
    platform_fmt_store_simple(button_flag, "Show help", Lui_context::SLOT_LABEL_HELP);
    platform_fmt_store_simple(button_flag, u8"◁", Lui_context::SLOT_LABEL_PREV);
    platform_fmt_store_simple(button_flag, u8"▷", Lui_context::SLOT_LABEL_NEXT);

    platform_fmt_init();
    platform_fmt_text(Text_fmt::BOLD, "Step-by-step");
    platform_fmt_text(Text_fmt::SMALL, "(Move using arrow keys)");
    platform_fmt_store(Lui_context::SLOT_BUTTON_DESC_CONTEXT);

    // Initialise elements
    context->elem_flags = array_create<u64> (Lui_context::SLOT_COUNT);
    context->elem_bb    = array_create<Rect>(Lui_context::SLOT_COUNT);
    
    context->entry_numbers.slot = Lui_context::SLOT_ENTRY_NUMBERS;
    context->entry_base.slot = Lui_context::SLOT_ENTRY_BASE;
    context->entry_bitorder.slot = Lui_context::SLOT_ENTRY_BITORDER;
    context->entry_firstnode.slot = Lui_context::SLOT_ENTRY_FIRSTNODE;
    context->entry_secondnode.slot = Lui_context::SLOT_ENTRY_SECONDNODE;

    context->elem_flags[Lui_context::SLOT_LABEL_NEXT] |= Lui_context::DRAW_COMPACT;
    context->elem_flags[Lui_context::SLOT_LABEL_PREV] |= Lui_context::DRAW_COMPACT;

    // Initialise application
    application_init();

    // Set initial radiobutton
    context->elem_flags[Lui_context::SLOT_LABEL_UNION] |= Lui_context::DRAW_PRESSED;
}
    
void platform_operations_disable() {return;
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
        Lui_context::SLOT_LABEL_FRAME,
        Lui_context::SLOT_LABEL_OP_U,
        Lui_context::SLOT_LABEL_OP_I,
        Lui_context::SLOT_LABEL_OP_C,
        Lui_context::SLOT_LABEL_REMOVEALL,
        Lui_context::SLOT_LABEL_PREV,
        Lui_context::SLOT_LABEL_NEXT,
        Lui_context::SLOT_ENTRY_FIRSTNODE,
        Lui_context::SLOT_ENTRY_SECONDNODE
    };

    for (s64 i: elem_disable) {
        context->elem_flags[i] |= Lui_context::DRAW_DISABLED;
    }
}

void _platform_frame_draw() {
    float ox = global_context.screen_w / 2.f;
    float oy = global_context.screen_h / 2.f;
    float sx =  2.f / global_context.screen_w;
    float sy = -2.f / global_context.screen_h;

    auto context = &global_platform.gl_context; // The macros expect a local named context

    if (context->prep_dirty) {
        context->prep_dirty = false;
        
        if (context->uitext_tex) {
            glDeleteTextures(1, &context->uitext_tex);
        }
        glGenTextures(1, &context->uitext_tex);
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D, context->uitext_tex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, context->prep_size, context->prep_size, 0, GL_RED, GL_UNSIGNED_BYTE, context->prep_image.data);
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

void _platform_render(Platform_state* platform) {
    assert(platform);
    
    application_render();
    glClear(GL_COLOR_BUFFER_BIT);
    glClear(GL_DEPTH_BUFFER_BIT);

    _platform_frame_init();
    
    Lui_context* context = &global_platform.gl_context;
    auto font_inst = context->fonts[Lui_context::FONT_LUI_NORMAL];

    // Process input
    
    auto in_rect = [](Rect r, s64 x, s64 y) {
        return r.x <= x and x < r.x+r.w and r.y <= y and y < r.y+r.h;
    };

    for (Key key: context->input_queue) {
        if (key.type == Key::SPECIAL and key.special == Key::C_QUIT) {
            exit(0);
        }

        if (key.type == Key::MOUSE) {
            u8 action; s64 x, y;
            key.get_mouse_param(&action, &x, &y);

            bool consumed = false;
            for (s64 slot = 0; slot < Lui_context::SLOT_COUNT; ++slot) {
                if (context->elem_flags[slot] & Lui_context::DRAW_DISABLED) continue;
                if (not in_rect(context->elem_bb[slot], context->pointer_x, context->pointer_y)) continue;
                
                if (action == Key::LEFT_DOWN) {
                    context->elem_flags[context->elem_focused] &= ~Lui_context::DRAW_FOCUSED;
                    context->elem_focused = slot;
                    context->elem_tabindex = slot;
                    context->elem_flags[slot] |= Lui_context::DRAW_PRESSED;
                    context->elem_flags[slot] |= Lui_context::DRAW_FOCUSED;

                    if (context->elem_flags[slot] & Lui_context::DRAW_RADIO) {
                        for (s64 slot_j = slot-1; context->elem_flags[slot_j] & Lui_context::DRAW_RADIO; --slot_j)
                            context->elem_flags[slot_j] &= ~Lui_context::DRAW_PRESSED;
                        for (s64 slot_j = slot+1; context->elem_flags[slot_j] & Lui_context::DRAW_RADIO; ++slot_j)
                            context->elem_flags[slot_j] &= ~Lui_context::DRAW_PRESSED;
                    }
                } else if (action == Key::LEFT_UP) {
                    if (~context->elem_flags[slot] & Lui_context::DRAW_RADIO) {
                        context->elem_flags[slot] &= ~Lui_context::DRAW_PRESSED;
                    }
                }
                
                consumed = true;
                break;
            }

            if (not consumed and action == Key::LEFT_DOWN) {
                context->elem_flags[context->elem_focused] &= ~Lui_context::DRAW_FOCUSED;
                context->elem_focused = 0;
                consumed = true;
            }

            if (consumed) continue;
        } else if (key.type == Key::SPECIAL) {
            bool consumed;
            if (key.special == Key::TAB or key.special == Key::SHIFT_TAB) {
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
                    consumed = true;
                }
            }

            if (consumed) continue;
        } 

        if (context->elem_flags[context->elem_focused] & Lui_context::DRAW_ENTRY) {
            Text_entry* entry;
            switch (context->elem_focused) {
            case Lui_context::SLOT_ENTRY_NUMBERS:    entry = &context->entry_numbers; break;
            case Lui_context::SLOT_ENTRY_BASE:       entry = &context->entry_base; break;
            case Lui_context::SLOT_ENTRY_BITORDER:   entry = &context->entry_bitorder; break;
            case Lui_context::SLOT_ENTRY_FIRSTNODE:  entry = &context->entry_firstnode; break;
            case Lui_context::SLOT_ENTRY_SECONDNODE: entry = &context->entry_secondnode; break;
            default: assert(false);
            }

            enum Move_mode: u8 {
                SINGLE, LINE, WORD, FOREVER
            };

            auto isalnum = [](u8 c) {
                return ('0' <= c and c <= '9') or ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z');
            };
            auto move_r = [entry, &isalnum](u8 mode) {
                while (entry->cursor < entry->text.size) {
                    if (entry->text[entry->cursor] == '\n') {
                        if (mode == LINE) break;
                        entry->cursor_col = -1;
                        ++entry->cursor_row;
                    }
                    do {
                        ++entry->cursor_col; ++entry->cursor;
                    } while (entry->cursor < entry->text.size and entry->text[entry->cursor] & 0x80);
                    if (mode == SINGLE) break;
                    if (mode == WORD and entry->cursor < entry->text.size and isalnum(entry->text[entry->cursor-1])
                        and not isalnum(entry->text[entry->cursor])) break;
                }
            };
            auto move_l = [entry, &isalnum](u8 mode) {
                while (entry->cursor > 0) {
                    if (mode == LINE and entry->text[entry->cursor-1] == '\n') break;
                    
                    do {
                        --entry->cursor_col; --entry->cursor;
                    } while (entry->cursor >= 0 and entry->text[entry->cursor] & 0x80);
                    
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
                    s64 c_len = _decode_utf8(array_subarray(entry->text, c_i, entry->text.size));
                    Text_box box;
                    lui_text_prepare_word(context, Lui_context::FONT_LUI_SANS, array_subarray(entry->text, c_i, c_i+c_len), &box);
                    width += (s64)std::round(box.advance);
                    c_i += c_len;
                }
                return width;
            };
            auto move_width = [context, entry](s64 width) {
                while (entry->cursor < entry->text.size and width > 0) {
                    if (entry->text[entry->cursor] == '\n') break;
                    
                    s64 c_len = _decode_utf8(array_subarray(entry->text, entry->cursor, entry->text.size));
                    Text_box box;
                    auto c_arr = array_subarray(entry->text, entry->cursor, entry->cursor+c_len);
                    lui_text_prepare_word(context, Lui_context::FONT_LUI_SANS, c_arr, &box);
                    s64 advance = (s64)std::round(box.advance);

                    if (advance >= 2 * width) break;

                    entry->cursor += c_len;
                    entry->cursor_col += c_len;
                    width -= advance;
                }
            };
            
            auto move_d = [context, entry, &get_width, &move_width](s64 amount) {
                assert(amount >= 0);

                s64 width = get_width();
                
                while (entry->cursor < entry->text.size and amount > 0) {
                    if (entry->text[entry->cursor] == '\n') {
                        --amount;
                        ++entry->cursor_row;
                        entry->cursor_col = -1;
                    }
                    ++entry->cursor;
                    ++entry->cursor_col;
                }
                
                move_width(width);
            };
            auto move_u = [context, entry, &get_width, &move_width](s64 amount) {
                assert(amount >= 0);

                s64 width = get_width();

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
                if (amount > 0) return;

                move_width(width);
            };

            bool consumed = true;
            if (key.type == Key::SPECIAL) {
                if (key.special == Key::ARROW_R) {
                    move_r(key.flags & Key::MOD_CTRL ? WORD : SINGLE);
                } else if (key.special == Key::ARROW_L) {
                    move_l(key.flags & Key::MOD_CTRL ? WORD : SINGLE);
                } else if (key.special == Key::ARROW_D and (~key.flags & Key::MOD_CTRL)) {
                    move_d(1);
                } else if (key.special == Key::ARROW_U and (~key.flags & Key::MOD_CTRL)) {
                    move_u(1);
                } else if (key.special == Key::HOME) {
                    move_l(key.flags & Key::MOD_CTRL ? FOREVER : LINE);
                } else if (key.special == Key::END) {
                    move_r(key.flags & Key::MOD_CTRL ? FOREVER : LINE);
                } else {
                    consumed = false;
                }
            }

            //if (consumed) {
            //    s64 width = get_width();
            //    
            //}

            if (consumed) continue;
        }
        
        ui_key_press(key);
    }
    context->input_queue.size = 0;
    
    bool cursor_is_text = false;
    for (s64 slot = 0; slot < Lui_context::SLOT_COUNT; ++slot) {
        if (context->elem_flags[slot] & Lui_context::DRAW_DISABLED) continue;
        if (in_rect(context->elem_bb[slot], context->pointer_x, context->pointer_y)) {
            context->elem_flags[slot] |= Lui_context::DRAW_ACTIVE;
            if (context->elem_flags[slot] & Lui_context::DRAW_ENTRY) {
                cursor_is_text = true;
            }
        } else {
            context->elem_flags[slot] &= ~Lui_context::DRAW_ACTIVE;
            // This is a bit hacky. Due to the way we only get the correct flags after drawing once,
            // we run the risk of resetting the initial radiobutton selection here.
            if (context->elem_flags[slot] & (Lui_context::DRAW_BUTTON | Lui_context::DRAW_ENTRY)) {
                context->elem_flags[slot] &= ~Lui_context::DRAW_PRESSED;
            }
        }
    }
    platform_set_cursor(cursor_is_text);
    
    // Now draw the UI
    
    u8 white[] = {255, 255, 255, 255};
    u8 black[] = {0, 0, 0, 255};
    lui_draw_rect(context, 0, 0, context->panel_left_width, global_context.screen_h, Lui_context::LAYER_BACK, white);

    s64 x = 10;
    s64 y = 10;
    s64 w = context->panel_left_width - 20;
    s64 w_line, ha_line;
    
    auto hsep = [context, x, w, &y, font_inst]() {
        u8 gray[] = {153, 153, 153, 255};
        lui_draw_rect(context, x + 12, y, w - 24, 1, Lui_context::LAYER_MIDDLE, gray);
        y += (s64)std::round(font_inst.newline*1.5f - font_inst.height + 1);
    };

    platform_fmt_draw(Lui_context::SLOT_INITTEXT, x, y, w, &x, &y);
    hsep();

    char const* str = "2, 4, 13, 17, 20, 24, 25, 31, 33, 41, 51, 52p, 61, 62\nA reaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaally loooooooooooooooooooooooong line\nthat should be clipped";
    context->entry_numbers.text = {(u8*)str, (s64)strlen(str)};
    lui_draw_entry(context, context->entry_numbers, x, y, w, 3, nullptr, &y, nullptr);
    y += std::round(font_inst.height - font_inst.ascent);

    {s64 x_orig = x, ha_line;
    lui_draw_entry(context, context->entry_base, x, y, (s64)std::round(font_inst.space*12.f), 1, nullptr, nullptr, &ha_line, true);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_LABEL_BASE, x, y, -1, &x, &y);
    y -= ha_line;
    lui_draw_entry(context, context->entry_base, x, y, (s64)std::round(font_inst.space*12.f), 1, &x, nullptr, nullptr);
    y += ha_line; x += 10;
    platform_fmt_draw(Lui_context::SLOT_LABEL_BITORDER, x, y, -1, &x, &y);
    y -= ha_line;
    lui_draw_entry(context, context->entry_bitorder, x, y, (s64)std::round(font_inst.space*30.f), 1, nullptr, &y, nullptr);
    y += (s64)std::round(font_inst.height - font_inst.ascent); x = x_orig;}
    
    {s64 w_line, ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_LABEL_CREATE, x, y, w, &ha_line, &w_line);
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
    lui_draw_entry(context, context->entry_firstnode, x, y, (s64)std::round(font_inst.space*12.f), 1, nullptr, nullptr, &ha_line, true);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_LABEL_FIRSTNODE, x, y, -1, &x, &y);
    y -= ha_line;
    lui_draw_entry(context, context->entry_firstnode, x, y, (s64)std::round(font_inst.space*12.f), 1, &x, nullptr, nullptr);
    y += ha_line; x += 10;
    platform_fmt_draw(Lui_context::SLOT_LABEL_SECONDNODE, x, y, -1, &x, &y);
    y -= ha_line;
    lui_draw_entry(context, context->entry_secondnode, x, y, (s64)std::round(font_inst.space*12.f), 1, nullptr, &y, nullptr);
    y += (s64)std::round(font_inst.height - font_inst.ascent); x = x_orig;}

    {s64 w_line, ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_LABEL_OP_U, x, y, w, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_OP, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    {s64 w_line, ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_LABEL_REMOVEALL, x, y, w, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_REMOVEALL, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    {s64 w_line, ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_LABEL_HELP, x, y, w, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_HELP, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}
    
    {s64 w_line, ha_line, w_label, pad_x = 5;
    lui_draw_button_right(context, Lui_context::SLOT_LABEL_NEXT, x, y, w, &ha_line, &w_line);
    context->lui_buffer.size = 0;
    array_printf(&context->lui_buffer, "%d/%d", 382, 929);
    platform_fmt_store_simple(Text_fmt::NOSPACE, context->lui_buffer, Lui_context::SLOT_LABEL_FRAME);
    platform_fmt_draw(Lui_context::SLOT_LABEL_FRAME, 0, 0, -1, &w_label, nullptr, true);
    y += ha_line; w_line -= w_label + pad_x;
    platform_fmt_draw(Lui_context::SLOT_LABEL_FRAME, x+w_line, y, -1, nullptr, nullptr);
    y -= ha_line;
    lui_draw_button_right(context, Lui_context::SLOT_LABEL_PREV, x, y, w_line-pad_x, nullptr, &w_line);
    y += ha_line; 
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_CONTEXT, x, y, w_line, &x, &y);}    
    
    _platform_frame_draw();
    glXSwapBuffers(platform->display, platform->window_glx);
}

void linux_get_event_key(Array_dyn<Key>* keys, XKeyEvent e) {
    KeySym keysym;

    // You would think that we could do some dynamic resizing here if the buffer is too
    // small. However, the API does not seem to support it.
    char buffer_[64];
    Array_t<char> buffer {buffer_, sizeof(buffer)};
    buffer.size = XLookupString(&e, buffer.data, buffer.size, &keysym, NULL);
    
    keys->size = 0;

    s64 special = Key::INVALID;
    u64 mod = 0;
    u64 shiftctrl = ShiftMask | ControlMask;
    switch (keysym) {
    case XK_Escape:       special = Key::ESCAPE; break;
    case XK_Page_Up:      special = Key::PAGE_U; break;
    case XK_Page_Down:    special = Key::PAGE_D; break;
    case XK_Tab:          special = Key::TAB; break;
    case XK_F1:           special = Key::F1; break;
    case XK_F2:           special = Key::F2; break;
    case XK_F3:           special = Key::F3; break;
    case XK_F4:           special = Key::F4; break;
    case XK_F5:           special = Key::F5; break;
    case XK_F6:           special = Key::F6; break;
    case XK_F7:           special = Key::F7; break;
    case XK_F8:           special = Key::F8; break;
    case XK_F9:           special = Key::F9; break;
    case XK_F10:          special = Key::F10; break;
    case XK_F11:          special = Key::F11; break;
    case XK_F12:          special = Key::F12; break;
    case XK_Left:         special = Key::ARROW_L; mod = e.state & shiftctrl; break;
    case XK_Right:        special = Key::ARROW_R; mod = e.state & shiftctrl; break;
    case XK_Down:         special = Key::ARROW_D; mod = e.state & shiftctrl; break;
    case XK_Up:           special = Key::ARROW_U; mod = e.state & shiftctrl; break;
    case XK_Home:         special = Key::HOME;    mod = e.state & shiftctrl; break;
    case XK_End:          special = Key::END;     mod = e.state & shiftctrl; break;
    case XK_ISO_Left_Tab: special = Key::SHIFT_TAB;   mod = ShiftMask; break;
    case XK_c:            special = Key::C_COPY;      mod = ControlMask; break;
    case XK_v:            special = Key::C_PASTE;     mod = ControlMask; break;
    case XK_a:            special = Key::C_SELECTALL; mod = ControlMask; break;
    case XK_q:            special = Key::C_QUIT;      mod = ControlMask; break;
    case XK_s:            special = Key::C_SAVE;      mod = ControlMask; break;
    case XK_z:            special = Key::C_UNDO;      mod = ControlMask; break;
    case XK_Z:            special = Key::C_REDO;      mod = ControlMask | ShiftMask; break;
    }

    u64 mod_mask = ShiftMask | LockMask | ControlMask | Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask;
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

void platform_set_cursor(bool is_text) {
    if (is_text == global_platform.cursor_is_text) return;

    global_platform.cursor_is_text = is_text;
    if (is_text) {
        XDefineCursor(global_platform.display, global_platform.window, global_platform.cursor_text);
    } else {
        XUndefineCursor(global_platform.display, global_platform.window);
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

int main(int argc, char** argv) {
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
        | ButtonReleaseMask | StructureNotifyMask | PointerMotionMask;
    
    Window window = XCreateWindow(display, DefaultRootWindow(display), 0, 0, 1300, 800, 0,
        visual->depth, InputOutput, visual->visual, CWColormap | CWEventMask, &window_attrs); // We pass a type of InputOutput explicitly, as visual->c_class is an illegal value for some reason. A good reason, I hope.
    global_platform.window = window;

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
    global_platform.cursor_text = XCreateFontCursor(display, 152);
    
    GLXWindow window_glx = glXCreateWindow(display, *config, window, nullptr);
    global_platform.window_glx = window_glx;

    // Map the context to the window
    glXMakeContextCurrent(display, window_glx, window_glx, context); // This returns a bool, but I cannot find what it means in the spec, so just ignore it. The greatness continues.

    // Do application-specific initialisation
    _platform_init(&global_platform);

    XMapWindow(display, window);

    bool redraw = false;
    while (true) {
        XEvent event;
        
        if (XPending(display) <= 0 and redraw) {
            redraw = false;
            _platform_render(&global_platform);
        }
        XNextEvent(display, &event);
        
        switch (event.type) {
        case Expose:
            redraw = true;
            break;
        case KeyPress:
            linux_get_event_key(&global_platform.gl_context.input_queue, event.xkey);
            redraw = true;
            break;
        case ButtonPress:
            if (event.xbutton.button == Button1) {
                Key key = Key::create_mouse(Key::LEFT_DOWN, event.xbutton.x, event.xbutton.y);
                array_push_back(&global_platform.gl_context.input_queue, key);
                redraw = true;
            }
            break;
        case ButtonRelease:
            if (event.xbutton.button == Button1) {
                Key key = Key::create_mouse(Key::LEFT_UP, event.xbutton.x, event.xbutton.y);
                array_push_back(&global_platform.gl_context.input_queue, key);
                redraw = true;
            }
            break;
        case MappingNotify:
            if (event.xmapping.request == MappingModifier or event.xmapping.request == MappingKeyboard) {
                XRefreshKeyboardMapping(&event.xmapping);
            }
            break;
        case ClientMessage:
            if (event.xclient.message_type == wm_protocols and event.xclient.data.l[0] - wm_delete_window == 0) {
                array_push_back(&global_platform.gl_context.input_queue, Key::create_special(Key::C_QUIT, 0));
                redraw = true;
            }
            break;
        case ConfigureNotify:
            _platform_handle_resize(event.xconfigure.width, event.xconfigure.height);
            break;
        case MotionNotify:
            global_platform.gl_context.pointer_x = event.xmotion.x;
            global_platform.gl_context.pointer_y = event.xmotion.y;
            redraw = true;
            break;
        default:
            break;
        }
    }

    // Memory freed by OS
}


