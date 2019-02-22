
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
        perror("Error:"); exit(1025);
    }

    if (fseek(f, 0, SEEK_END) == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (2)\n", path.data);
        perror("Error:"); exit(1026);
    }

    s64 f_size = ftell(f);
    if (f_size == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (3)\n", path.data);
        perror("Error:"); exit(1027);
    }

    if (fseek(f, 0, SEEK_SET) == -1) {
        fprintf(stderr, "Error: Could not open file '%s' for reading (4)\n", path.data);
        perror("Error:"); exit(1028);
    }

    Array_t<u8> data = array_create<u8>(f_size);
    s64 read = fread(data.data, 1, data.size, f);
    if (read < f_size) {
        if (ferror(f)) {
            fprintf(stderr, "Error: Could not open file '%s' for reading (5)\n", path.data);
            perror("Error:"); exit(1029);
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

struct Text_box {
    enum Flags: u8 {
        NEWLINE = 1, NEWPAR = 2, NEWTEXT = 4
    };
    
    float x0 = 0.f, y0 = 0.f, x1 = 0.f, y1 = 0.f;
    float s0 = 0.f, t0 = 0.f, s1 = 0.f, t1 = 0.f;
    float advance = 0.f;
    u8 flags = 0;
}; //@Cleanup: Move

// Keeps the necessary data to manage OpenGL data for the platform layer
struct Platform_gl_context {
    // Indices for all the vertex attributes
    enum Attributes: GLuint {
        UIRECT_POS = 0, UIRECT_FILL, UIRECT_ATTR_COUNT,
        UITEXT_POS = 0, UITEXT_TPOS, UITEXT_FILL, UITEXT_ATTR_COUNT,
    };
    // Names for all uniforms
    enum Uniforms: int {
        UIRECT_ORIGIN, UIRECT_SCALE,
        UITEXT_ORIGIN, UITEXT_SCALE, UITEXT_SAMPLER,
        UNIFORMS_COUNT
    };
    
    // Ids of the shader programs
    GLuint program_uirect;
    GLuint program_uitext;
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

    // Names of the buffers for the vertex attributes of each shader
    Array_t<GLuint> buffers_uirect;
    Array_t<GLuint> buffers_uitext;

    // Font data
    Array_t<u8> font_ui_data;
    stbtt_fontinfo font_ui;
    float font_ui_scale;
};

struct Platform_state {
    bool is_active = true;
    Array_dyn<Key> input_queue;

    s64 panel_left_width = 450; //@Cleanup: Make this DPI aware
    
    Display* display = nullptr;
    GLXWindow window_glx;

    Platform_gl_context gl_context;
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

//int platform_text_prepare(int size_pixel, Array_t<u8> chars, Array_t<Rect>* out_positions) {
//    if (global_platform.font_labels_size != size_pixel) {
//        
//    }
//    
//    return 0;
//}

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


void uil_text_prepare(Platform_gl_context* context, Array_t<u8> s, Array_dyn<Text_box>* boxes); //@Cleanup: Remove

void _platform_init(Platform_state* platform) {
    assert(platform);
    
    _platform_init_opengl();

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
        "    gl_Position = vec4((pos - origin)*scale, 0.09, 1);\n"
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

    // Font stuff
    //context->font_ui_data = array_load_from_file("OpenSans-Regular.ttf");
    context->font_ui_data = array_load_from_file("DejaVuSerif.ttf");
    int code = stbtt_InitFont(&context->font_ui, context->font_ui_data.data, 0);
    if (code == 0) {
        fprintf(stderr, "Error: Could not parse font data\n");
        exit(101);
    }

    context->font_ui_scale = stbtt_ScaleForPixelHeight(&context->font_ui, 20); //@Cleanup: Choose font size so that borders of x are pixels
    char const* s = "This is a test. Please stand by. I am not sure this will work. Paragraph wrapping is a hard problem,";
    uil_text_prepare(context, {(u8*)s, (s64)strlen(s)}, &context->uitext_boxes);
    
    //@Cleanup: Check max size using RECTANGLE_TEXTURE_SIZE
}

void uil_draw_rect(Platform_gl_context* context, s64 x, s64 y, s64 w, s64 h, s64 layer, u8* fill) {
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

void uil_text_prepare(Platform_gl_context* context, Array_t<u8> s, Array_dyn<Text_box>* boxes) {
    assert(context and boxes);
    boxes->size = 0;

    // I expect the text that comes in here to only contain zeros, printables, spaces and newlines.

    s64 i = 0;
    while (s[i] == '\n' or s[i] == ' ') ++i;

    s64 size = 512;
    Array_t<u8> image = array_create<u8>(size * size);
    s64 x = 0, y = 0, y_incr = 0;
    
    Array_dyn<s32> glyphs;
    Array_dyn<u8> buf;
    defer { array_free(&glyphs); };
    defer { array_free(&buf); };

    float f = context->font_ui_scale;
    
    while (i < s.size) {
        Text_box box;
        
        s64 j;
        for (j = i; j < s.size; ++j) {
            if (s[j] == ' ' or s[j] == '\n' or s[j] == 0) break;
        }
        s64 next;
        for (next = j; next < s.size and (s[next] == ' ' or s[next] == '\n'); ++next) {
            if (s[next] == '\n') {
                box.flags = box.flags & Text_box::NEWLINE ? Text_box::NEWPAR : Text_box::NEWLINE;
            }
        }

        if (next < s.size and s[next] == 0) box.flags |= Text_box::NEWTEXT;

        glyphs.size = 0;
        while (i < j) {
            // Decode utf-8
            u32 c = s[i];
            s64 c_bytes = c&128 ? c&64 ? c&32 ? c&16 ? 4 : -1 : 3 : 2 : 1;
            if (c_bytes == 1) {
                // nothing
            } else if (c_bytes == 2) {
                c = (s[i]&0x1f) << 6 | (s[i+1]&0x3f);
            } else if (c_bytes == 3) {
                c = (s[i]&0xf) << 12 | (s[i+1]&0x3f) << 6 | (s[i+2]&0x3f);
            } else if (c_bytes == 4) {
                c = (s[i]&0x7) << 18 | (s[i+1]&0x3f) << 12 | (s[i+2]&0x3f) << 6 | (s[i+3]&0x3f);
            } else {
                assert(false);
            }
            
            s32 glyph = stbtt_FindGlyphIndex(&context->font_ui, c);
            if (glyph) array_push_back(&glyphs, glyph);
            
            i += c_bytes;
        }
        i = next;

        float shift = 0.f;
        bool draw = false;
        s64 x_orig = x;
        s64 y_orig = y;
        s64 x_init_off = 0;
        s64 y_incr_new = 0;
        
        for (s64 k = 0; k < glyphs.size; ++k) {
            int ix0, iy0, ix1, iy1;
            stbtt_GetGlyphBitmapBoxSubpixel(&context->font_ui, glyphs[k], f, f, shift, 0.f, &ix0, &iy0, &ix1, &iy1);

            s64 w = ix1 - ix0;
            s64 h = iy1 - iy0;

            int adv, lsb;
            stbtt_GetGlyphHMetrics(&context->font_ui, glyphs[k], &adv, &lsb);

            s64 x0 = x + (s64)std::round(shift + lsb*f);
            s64 y0 = y + iy0;

            if (not draw) {
                if (x0 < x_orig+1) {
                    x_init_off += x_orig - x0;
                    x += x_orig - x0;
                    x0 = x_orig;
                }
                if (y0 < y_orig+1) {
                    y += y_orig+1 - y0;
                    y0 = y_orig+1;
                }
                if (y_incr_new < y0+h - y_orig) y_incr_new = y0+h - y_orig;
            } else {
                array_resize(&buf, w * h);
                stbtt_MakeGlyphBitmapSubpixel(&context->font_ui, buf.data, w, h, w, f, f, shift, 0.f, glyphs[k]);

                for (s64 row = 0; row < h; ++row) {
                    for (s64 col = 0; col < w; ++col) {
                        u8* p = &image[(x0 + col) + (y0 + row) * size];
                        *p = (u8)std::min(255, *p + buf[col + row*w]);
                    }
                }
            }

            if (k+1 < glyphs.size) {
                int kern = stbtt_GetGlyphKernAdvance(&context->font_ui, glyphs[k], glyphs[k+1]);
                shift += (adv + kern) * f;
                float shift_f = std::floor(shift);
                x += (s64)shift_f;
                shift -= shift_f;
            } else if (not draw) {
                // Execute the loop another time, but now we draw the glyphs
                draw = true;
                k = -1;
                shift = 0.f;
                box.x0 = (float)(-x_init_off);
                box.y0 = (float)(y_orig+1 - y);
                box.x1 = (float)(x0 + w - x_orig - x_init_off);
                box.y1 = (float)(y_incr_new - y + y_orig+1);
                box.advance = (float)(x - x_orig - x_init_off) + adv*f;
                
                if (x0 + w < size) {
                    x = x_orig + x_init_off;
                    if (y_incr < y_incr_new) y_incr = y_incr_new;
                } else {
                    x = x_init_off;
                    y += y_incr;
                    y_orig += y_incr;
                    y_incr = y_incr_new;
                }
                
                box.s0 = (float)(box.x0 + x) / (float)size;
                box.t0 = (float)(box.y0 + y) / (float)size;
                box.s1 = (float)(box.x1 + x) / (float)size;
                box.t1 = (float)(box.y1 + y) / (float)size;
            } else {
                x = x0 + w;
                y = y_orig;
            }
        }

        array_push_back(boxes, box);
    }

    if (context->uitext_tex != 0) glDeleteTextures(1, &context->uitext_tex);
    glGenTextures(1, &context->uitext_tex);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, context->uitext_tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, size, size, 0, GL_RED, GL_UNSIGNED_BYTE, image.data);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
}

void uil_text_draw(Platform_gl_context* context, Array_t<Text_box> boxes, s64 x_, s64 y_, s64 w_, u8* fill) {
    assert(context);
    
    float x = (float)x_, y = (float)y_, w = (float)w_;
    y = global_context.screen_h-1 - y;
    float orig_x = x, orig_y = y;

    int ascent_, descent_, linegap_;
    stbtt_GetFontVMetrics(&context->font_ui, &ascent_, &descent_, &linegap_);
    float f = context->font_ui_scale;
    float ascent  = ascent_ *f, descent = descent_*f, linegap = linegap_*f;

    float newline_off = ascent - descent + linegap;
    float newpar_off  = newline_off * 1.5;

    float newword_off;
    {int adv, lsb;
    stbtt_GetCodepointHMetrics(&context->font_ui, ' ', &adv, &lsb);
    newword_off = (float)adv * f;}
    
    y += ascent;
    u8 flags = 0;

    for (Text_box box: boxes) {
        if (flags & Text_box::NEWLINE) {
            x = orig_x;
            y += newpar_off;
        } else if (x + box.x1 > w or (flags & Text_box::NEWLINE)) {
            x = orig_x;
            y += newline_off;
        }
        if (x + box.x0 < 0) {
            x = -box.x0;
        }

        flags = box.flags;
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

        x += box.advance + newword_off;
    }
}


/*void uil_draw_text_multiline(Platform_gl_context* context, s64 x_, s64 y_, s64 w_, Array_t<u8> text, u8* fill) {
    float x = (float)x_, y = (float)y_, w = (float)w_;
    y = global_context.screen_h - y;
    float orig_x = x, orig_y = y;

    int ascent_, descent_, linegap_;
    stbtt_GetFontVMetrics(&context->font_ui, &ascent_, &descent_, &linegap_);
    float f = context->font_ui_scale;
    float ascent  = ascent_ *f, descent = descent_*f, linegap = linegap_*f;
    
    y += ascent;

    for (u8 c: text) {
        int x0_, y0_, x1_, y1_;
        stbtt_GetCodepointBox(&context->font_ui, c, &x0_, &y0_, &x1_, &y1_);
        float x0 = x0_*f, y0 = y0_*f, x1 = x1_*f, y1 = y1_*f;

        if (x + x1 > w) {
            x = orig_x;
            y += ascent - descent + linegap;
        }
        if (x + x0 < 0) {
            x = -x0;
        }
        
        stbtt_aligned_quad q;
        stbtt_GetPackedQuad(context->uitext_pack.data, 512, 512, c - 0x20, &x, &y, &q, true);
        array_append(&context->buf_uitext_pos, {
            q.x0, q.y0, q.x1, q.y0, q.x1, q.y1, q.x0, q.y0, q.x1, q.y1, q.x0, q.y1
        });
        array_append(&context->buf_uitext_tpos, {
            q.s0, q.t0, q.s1, q.t0, q.s1, q.t1, q.s0, q.t0, q.s1, q.t1, q.s0, q.t1
        });

        for (s64 j = 0; j < 6; ++j) {
            array_append(&context->buf_uitext_fill, {fill, 4});
        }
    }
    }*/

void _platform_frame_init() {
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
    auto context = &global_platform.gl_context;
    
    context->buf_uirect_pos.size = 0;
    context->buf_uirect_fill.size = 0;

    context->buf_uitext_pos.size = 0;
    context->buf_uitext_tpos.size = 0;
    context->buf_uitext_fill.size = 0;
}

void _platform_frame_draw() {
    float ox = global_context.screen_w / 2.f;
    float oy = global_context.screen_h / 2.f;
    float sx =  2.f / global_context.screen_w;
    float sy = -2.f / global_context.screen_h;

    auto context = &global_platform.gl_context; // The macros expect a local named context
    
    glUseProgram(context->program_uirect);
    
    OBST_DO_UNIFORM(UIRECT_ORIGIN, 2f, ox, oy);
    OBST_DO_UNIFORM(UIRECT_SCALE, 2f, sx, sy);

    OBST_DO_BUFFER(uirect, UIRECT_POS,  buf_uirect_pos,  3, GL_FLOAT, 0);
    OBST_DO_BUFFER(uirect, UIRECT_FILL, buf_uirect_fill, 4, GL_UNSIGNED_BYTE, 1);

    glDrawArrays(GL_TRIANGLES, 0, context->buf_uirect_pos.size / 3);

    glDisable(GL_DEPTH_TEST);
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
    
    // Now draw the UI
    u8 white[] = {255, 255, 255, 255};
    u8 black[] = {0, 0, 0, 255};
    uil_draw_rect(&platform->gl_context, 0, 0, platform->panel_left_width, global_context.screen_h, 1, white);

    /*char* s = "This is obst, AV, a visualisation of algorithms related to Binary Decision Diagrams, written by Philipp Czerner in 2018. Read to the help for more information, or get started right away by pressing \"Create and add\". Hint: You can hover over nodes using your cursor, showing additional details.";
      uil_draw_text_multiline(&platform->gl_context, 10, global_context.screen_h-10, platform->panel_left_width-20, {(u8*)s, (s64)strlen(s)}, black);*/

    uil_text_draw(&platform->gl_context, platform->gl_context.uitext_boxes, 10, global_context.screen_h-10, platform->panel_left_width-20, black);

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


