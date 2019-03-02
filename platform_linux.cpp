
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
    HEADER = 4, // Corresponds to <h4>, draw text as title
    BOLD = 8,
    ITALICS = 16,
    SMALL = 32,
    COMPACT = 64,
    GROUP_SPACING = PARAGRAPH | NEWLINE,
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
};

// Keeps the necessary data to manage OpenGL and other data for the uil layer
struct Lui_context {
    // Indices for all the vertex attributes
    enum Attributes: GLuint {
        UIRECT_POS = 0, UIRECT_FILL, UIRECT_ATTR_COUNT,
        UITEXT_POS = 0, UITEXT_TPOS, UITEXT_FILL, UITEXT_ATTR_COUNT,
        UIBUTTON_POS = 0, UIBUTTON_X, UIBUTTON_SIZE, UIBUTTON_BORDER, UIBUTTON_GRAD, UIBUTTON_CIRCLE, UIBUTTON_ATTR_COUNT,
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
    Array_dyn<float> buf_uibutton_grad;
    Array_dyn<float> buf_uibutton_circle;

    // Names of the buffers for the vertex attributes of each shader
    Array_t<GLuint> buffers_uirect;
    Array_t<GLuint> buffers_uitext;
    Array_t<GLuint> buffers_uibutton;

    // Date for font rendering
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
        SLOT_LABEL_BITORDER, SLOT_LABEL_OPERATION, SLOT_LABEL_UNION, SLOT_LABEL_INTERSECTION,
        SLOT_LABEL_COMPLEMENT, SLOT_LABEL_FIRSTNODE, SLOT_LABEL_SECONDNODE,
        SLOT_COUNT
    };
    
    // Data for formatted text display
    u64 fmt_flags; // Current flags
    Array_dyn<Text_box> fmt_boxes; // Set of boxes we are currently generating
    Array_dyn<Array_dyn<Text_box>> fmt_slots; // Stored sets of boxes

    enum Button_flags: u8 {
        BUTTON_NORMAL = 0,
        BUTTON_FOCUSED = 1,
        BUTTON_PRESSED = 2,
        BUTTON_ENTRY = 4,
        BUTTON_RADIO = 8,
    };
    
    Text_entry entry_numbers, entry_base, entry_bitorder, entry_firstnode, entry_secondnode;
};

struct Platform_state {
    bool is_active = true;
    Array_dyn<Key> input_queue;

    s64 panel_left_width = 470; //@Cleanup: Make this DPI aware
    
    Display* display = nullptr;
    GLXWindow window_glx;

    Lui_context gl_context;
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
void platform_operations_disable() {}

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
    
    global_context.width = std::min(global_context.screen_w - global_platform.panel_left_width, 800ll);
    global_context.height = global_context.screen_h;
    global_context.canvas_x = global_platform.panel_left_width;
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
        "attribute vec4 grad;\n"
        "attribute vec2 circle;\n"
        "varying vec2 v_p;\n"
        "varying float v_size;\n"
        "varying vec3 v_border;\n"
        "varying vec4 v_grad;\n"
        "varying vec2 v_circle;\n"
        "uniform vec2 origin;\n"
        "uniform vec2 scale;\n"
        "void main() {\n"
        "    gl_Position = vec4((pos - origin)*scale, 0.09, 1);\n"
        "    v_p = x;\n"
        "    v_size = size;\n"
        "    v_border = border;\n"
        "    v_grad = grad;\n"
        "    v_circle = circle;\n"
        "}\n";
    
    GLbyte shader_f_uibutton[] =
        "varying vec2 v_p;\n"
        "varying float v_size;\n"
        "varying vec3 v_border;\n"
        "varying vec4 v_grad;\n"
        "varying vec2 v_circle;\n"
        "void main() {\n"
        "    float dp = v_circle[0];\n"
        "    float f = pow(abs(v_p.x)/v_size, dp) + pow(abs(v_p.y)/v_size, dp) - 1.0;\n"
        "    vec2 df = vec2(dp/v_size*pow(abs(v_p.x)/v_size, dp-1.0), dp/v_size*pow(abs(v_p.y)/v_size, dp-1.0));\n"
        "    float d = f / length(df);\n"
        "    if (0.0 < d && d < 3.0) {\n"
        "        vec2 pp = v_p - f * df / dot(df, df) * vec2(sign(v_p.x), sign(v_p.y));\n"
        "        float f2 = pow(abs(pp.x)/v_size, dp) + pow(abs(pp.y)/v_size, dp) - 1.0;\n"
        "        vec2 df2 = vec2(dp/v_size*pow(abs(pp.x)/v_size, dp-1.0), dp/v_size*pow(abs(pp.y)/v_size, dp-1.0));\n"
        "        d += f2 / length(df2);\n"
        "    }\n"
        "    \n"
        "    float t = (v_p.y/v_size+1.0) / 2.0;\n"
        "    float i = mix(v_border[1], v_border[2], clamp(t, 0.0, 1.0));\n"
        "    float l = t < v_grad[0] ? mix(v_grad[1], v_grad[2], t / v_grad[0])\n"
        "            : mix(v_grad[1], v_grad[2], t / v_grad[0]);\n"
        "    if (d > v_border[0] + 0.5) discard;\n"
        "    float c1 = mix(l, i, clamp(d+0.5, 0.0, 1.0));\n"
        "    float c2 = mix(c1*v_circle[1], c1, clamp(length(v_p) - v_size*0.5 + 0.5, 0.0, 1.0));\n"
        "    float a = 1.0 - max(0.0, d - v_border[0] + 0.5);\n"
        "    gl_FragColor = vec4(c2, c2, c2, a);\n"
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
    OBST_ATTRIB(UIBUTTON_GRAD, grad);
    OBST_ATTRIB(UIBUTTON_CIRCLE, circle);
    OBST_PROGRAM_LINK(uibutton);
    OBST_UNIFORM(UIBUTTON_ORIGIN, origin);
    OBST_UNIFORM(UIBUTTON_SCALE, scale);

    OBST_GEN_BUFFERS(uibutton, UIBUTTON);
    
    //@Cleanup: Check max size using RECTANGLE_TEXTURE_SIZE
}

void lui_draw_rect(Lui_context* context, s64 x, s64 y, s64 w, s64 h, s64 layer, u8* fill) {
    float x1 = x,   y1 = y;
    float x2 = x+w, y2 = y+h;
    float z = 0.1f + (float)layer * 0.01f;
    
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

void lui_text_draw(Lui_context* context, Array_t<Text_box> boxes, s64 x_, s64 y_, s64 w_, u8* fill, s64* x_out, s64* y_out) {
    assert(context and x_ and y_);

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

        x = std::round(x + box.advance + font_inst.space);
        
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

struct Rect {
    s64 x, y, w, h;
};
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
    float border_thick [] = { 1.5, 0.62, 0.45 };
    float border_entry [] = { 1.0, 0.55, 0.72 };
    float grad_normal  [] = { 0.85, 1.0, 0.92, 0.95 };
    float grad_pressed [] = { 0.15, 0.9, 0.87, 0.95 };
    float grad_light   [] = { grad_light_t, 0.95, 1.0, 1.0 };

    bool inwards = flags & (Lui_context::BUTTON_ENTRY | Lui_context::BUTTON_RADIO);
    float* border = inwards ? border_entry :
        flags & (Lui_context::BUTTON_FOCUSED | Lui_context::BUTTON_PRESSED)
        ? border_thick : border_normal;
    float* grad = inwards ? grad_light :
        flags & Lui_context::BUTTON_PRESSED ? grad_pressed : grad_normal;
    float size = inwards ? h * 0.5f - 0.15f : h * 0.5f;
    float inner = inwards and (flags & Lui_context::BUTTON_PRESSED) ? 0.2f : 1.f;
    float circle = flags & Lui_context::BUTTON_RADIO ? 2.f : 8.f;
    
    
    for (s64 i = 0; i < 10; ++i) {
        array_push_back(&context->buf_uibutton_size, size);
        array_append(&context->buf_uibutton_border, {border, 3});
        array_append(&context->buf_uibutton_grad, {grad, 4});
        array_append(&context->buf_uibutton_circle, {circle, inner});
    }
}

void lui_draw_button_right(Lui_context* context, Array_t<u8> text, s64 x, s64 y, s64 w, u8 flags, s64* ha_out, s64* w_out) {
    assert(context and x and y and w);
    
    Text_box box;
    lui_text_prepare_word(context, Lui_context::FONT_LUI_SANS, text, &box, 0.9f);

    Padding pad {12, 4, 3, 3};
    Rect bb;
    bb.w = 2*pad.mar_x + 2*pad.pad_x + (s64)std::round(box.advance);
    bb.h = 2*pad.mar_y + 2*pad.pad_y + (s64)std::round(context->fonts[box.font].height);
    bb.x = x+w - bb.w;
    bb.y = y;
    
    Rect text_bb;
    lui_draw_buttonlike(context, bb, pad, flags, &text_bb);
    
    u8 gray[] = {70, 70, 70, 255};
    lui_text_draw(context, {&box, 1}, text_bb.x, text_bb.y, -1, gray, &text_bb.x, &text_bb.y);

    if (ha_out) *ha_out = text_bb.y - y + context->fonts[box.font].ascent - context->fonts[Lui_context::FONT_LUI_NORMAL].ascent;
    if (w_out) *w_out = w - bb.w;
}
void lui_draw_button_right(Lui_context* context, const char* text, s64 x, s64 y, s64 w, u8 flags, s64* ha_out, s64* w_out) {
    lui_draw_button_right(context, {(u8*)text, (s64)strlen(text)}, x, y, w, flags, ha_out, w_out);
}

void lui_draw_entry_text(Lui_context* context, Text_entry entry, Rect text_bb, u8 font) {
    auto font_inst = context->fonts[font];
    u8 fill[] = {20, 20, 20, 255};
    
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
            if (c_i >= entry.text.size) break;
            if (entry.text[c_i] == '\n') break;
            // Not entirely accurate, due to left-side-bearing, but should not matter
            if (x >= tx + tw) {
                while (c_i < entry.text.size and entry.text[++c_i] != '\n');
                break;
            }

            u32 c;
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

void lui_draw_entry(Lui_context* context, Text_entry entry, s64 x, s64 y, s64 w, s64 rows, s64* x_out, s64* y_out, s64* ha_out, bool only_measure=false) {
    u8 font = Lui_context::FONT_LUI_SANS;
    auto font_inst = context->fonts[font];
    Padding pad {7, 5, 3, 3};

    Rect bb {x, y, w, -1};
    bb.h = rows * (s64)std::round(font_inst.newline) + pad.mar_y*2 + pad.pad_y*2;
    
    Rect text_bb;
    lui_draw_buttonlike(context, bb, pad, Lui_context::BUTTON_ENTRY, &text_bb, only_measure);

    if (not only_measure) {
        lui_draw_entry_text(context, entry, text_bb, font);
    }
    
    if (x_out) *x_out = bb.x + bb.w;
    if (y_out) *y_out = bb.y + bb.h;
    if (ha_out) *ha_out = text_bb.y - y + font_inst.ascent - context->fonts[Lui_context::FONT_LUI_NORMAL].ascent;
}

void lui_draw_radio(Lui_context* context, s64 x, s64 y, s64* x_out) {
    auto font_inst = context->fonts[Lui_context::FONT_LUI_NORMAL];

    s64 size = 21;
    Padding pad {3, 3, 5, 5};
    Rect bb {x, y + (s64)std::round(font_inst.ascent) - size + pad.mar_y, size, size};
    lui_draw_buttonlike(context, bb, pad, Lui_context::BUTTON_RADIO | Lui_context::BUTTON_PRESSED, nullptr);

    if (x_out) *x_out += size;
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
    } else {
        font = Lui_context::FONT_LUI_NORMAL;
    }

    float letter_fac = flags & Text_fmt::COMPACT ? 0.95f : 1.f;
    
    s64 last = 0;
    for (s64 i = 0; i <= text.size; ++i) {
        if (i < text.size and text[i] != ' ' and text[i] != '\n') continue;
        
        if (last == i) {
            last = i;
            continue;
        }

        Text_box box;
        lui_text_prepare_word(&global_platform.gl_context, font, array_subarray(text, last, i), &box, letter_fac);
        array_push_back(&global_platform.gl_context.fmt_boxes, box);
        last = i+1;
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
void platform_fmt_draw(s64 slot, s64 x, s64 y, s64 w, s64* x_out, s64* y_out) {
    Lui_context* context = &global_platform.gl_context;
    lui_text_draw(context, context->fmt_slots[slot], x, y, w, nullptr, x_out, y_out);
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
    platform_fmt_text(Text_fmt::PARAGRAPH, "This is obst, a visualisation of algorithms related to Binary Decision Diagrams, written by Philipp Czerner in 2018");
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
    
    platform_fmt_init();
    platform_fmt_text(Text_fmt::BOLD, "Step-by-step");
    platform_fmt_text(Text_fmt::SMALL, "(Move using arrow keys)");
    platform_fmt_store(Lui_context::SLOT_BUTTON_DESC_CONTEXT);
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
    
    //application_render();
    glClear(GL_COLOR_BUFFER_BIT);
    glClear(GL_DEPTH_BUFFER_BIT);

    _platform_frame_init();
    
    Lui_context* context = &global_platform.gl_context;
    auto font_inst = context->fonts[Lui_context::FONT_LUI_NORMAL];
    
    // Now draw the UI
    
    u8 white[] = {255, 255, 255, 255};
    u8 black[] = {0, 0, 0, 255};
    lui_draw_rect(context, 0, 0, platform->panel_left_width, global_context.screen_h, 1, white);

    s64 x = 10;
    s64 y = 10;
    s64 w = platform->panel_left_width - 20;
    s64 w_line, ha_line;
    
    auto hsep = [context, x, w, &y, font_inst]() {
        u8 gray[] = {153, 153, 153, 255};
        lui_draw_rect(context, x + 12, y, w - 24, 1, 0, gray);
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
    lui_draw_button_right(context, "Create and add", x, y, w, 0, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_CREATE, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    {s64 x_orig = x;
    platform_fmt_draw(Lui_context::SLOT_LABEL_OPERATION, x, y, -1, &x, &y);
    lui_draw_radio(context, x, y, &x);
    platform_fmt_draw(Lui_context::SLOT_LABEL_UNION, x, y, -1, &x, &y);
    lui_draw_radio(context, x, y, &x);
    platform_fmt_draw(Lui_context::SLOT_LABEL_INTERSECTION, x, y, -1, &x, &y);
    lui_draw_radio(context, x, y, &x);
    platform_fmt_draw(Lui_context::SLOT_LABEL_COMPLEMENT, x, y, -1, &x, &y);
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
    lui_draw_button_right(context, "Calculate Union", x, y, w, 0, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_OP, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    {s64 w_line, ha_line;
    lui_draw_button_right(context, "Remove all", x, y, w, 0, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_REMOVEALL, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    {s64 w_line, ha_line;
    lui_draw_button_right(context, "Show help", x, y, w, 0, &ha_line, &w_line);
    y += ha_line;
    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_HELP, x, y, w_line, &x, &y);
    y += ha_line;
    hsep();}

    platform_fmt_draw(Lui_context::SLOT_BUTTON_DESC_CONTEXT, x, y, -1, &x, &y);
    
    _platform_frame_draw();
    glXSwapBuffers(platform->display, platform->window_glx);
}

void _platform_handle_keys(Platform_state* platform) {
    for (Key key: platform->input_queue) {
        if (key.type == Key::SPECIAL and key.special == Key::C_QUIT) {
            exit(0);
        }
        
        ui_key_press(key);
    }
    platform->input_queue.size = 0;
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
    switch (keysym) {
    case XK_Escape:    special = Key::ESCAPE; break;
    case XK_Left:      special = Key::ARROW_L; break;
    case XK_Right:     special = Key::ARROW_R; break;
    case XK_Down:      special = Key::ARROW_D; break;
    case XK_Up:        special = Key::ARROW_U; break;
    case XK_Home:      special = Key::HOME; break;
    case XK_End:       special = Key::END; break;
    case XK_Page_Up:   special = Key::PAGE_U; break;
    case XK_Page_Down: special = Key::PAGE_D; break;
    case XK_F1:        special = Key::F1; break;
    case XK_F2:        special = Key::F2; break;
    case XK_F3:        special = Key::F3; break;
    case XK_F4:        special = Key::F4; break;
    case XK_F5:        special = Key::F5; break;
    case XK_F6:        special = Key::F6; break;
    case XK_F7:        special = Key::F7; break;
    case XK_F8:        special = Key::F8; break;
    case XK_F9:        special = Key::F9; break;
    case XK_F10:       special = Key::F10; break;
    case XK_F11:       special = Key::F11; break;
    case XK_F12:       special = Key::F12; break;
    case XK_c:         special = Key::C_COPY;      mod = ControlMask; break;
    case XK_v:         special = Key::C_PASTE;     mod = ControlMask; break;
    case XK_a:         special = Key::C_SELECTALL; mod = ControlMask; break;
    case XK_q:         special = Key::C_QUIT;      mod = ControlMask; break;
    case XK_s:         special = Key::C_SAVE;      mod = ControlMask; break;
    case XK_z:         special = Key::C_UNDO;      mod = ControlMask; break;
    case XK_Z:         special = Key::C_REDO;      mod = ControlMask | ShiftMask; break;
    }

    u64 mod_mask = ShiftMask | LockMask | ControlMask | Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask | Mod5Mask;
    if (special != Key::INVALID and (e.state & mod_mask) == mod) {
        array_push_back(keys, Key::create_special(special));
    } else {
        s64 chunk = sizeof(Key::text);
        for (s64 i = 0; i < buffer.size; i += chunk) {
            s64 end = i + chunk < buffer.size ? i + chunk : buffer.size;
            array_push_back(keys, Key::create_text(array_subarray(buffer, i, end)));
        }
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
    window_attrs.event_mask = ExposureMask | KeyPressMask | KeyReleaseMask | StructureNotifyMask;
    
    Window window = XCreateWindow(display, DefaultRootWindow(display),
        0, 0, 1300, 800, 0,
    visual->depth, InputOutput, visual->visual, CWColormap | CWEventMask, &window_attrs); // We pass a type of InputOutput explicitly, as visual->c_class is an illegal value for some reason. A good reason, I hope.

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
    
    GLXWindow window_glx = glXCreateWindow(display, *config, window, nullptr);
    global_platform.window_glx = window_glx;

    // Map the context to the window
    glXMakeContextCurrent(display, window_glx, window_glx, context); // This returns a bool, but I cannot find what it means in the spec, so just ignore it. The greatness continues.

    // Do application-specific initialisation
    _platform_init(&global_platform);

    XMapWindow(display, window);
    
    while (true) {
        XEvent event;
        XNextEvent(display, &event);
        
        switch (event.type) {
        case Expose:
            _platform_render(&global_platform);
            break;
        case KeyPress:
            linux_get_event_key(&global_platform.input_queue, event.xkey);
            _platform_handle_keys(&global_platform);
            _platform_render(&global_platform);
            break;
        case MappingNotify:
            if (event.xmapping.request == MappingModifier or event.xmapping.request == MappingKeyboard) {
                XRefreshKeyboardMapping(&event.xmapping);
            }
            break;
        case ClientMessage:
            if (event.xclient.message_type == wm_protocols and event.xclient.data.l[0] - wm_delete_window == 0) {
                array_push_back(&global_platform.input_queue, Key::create_special(Key::C_QUIT));
                _platform_handle_keys(&global_platform);
            }
            break;
        case ConfigureNotify:
            _platform_handle_resize(event.xconfigure.width, event.xconfigure.height);
            break;
        default:
            break;
        }
    }

    // Memory freed by OS
}


