
// Written by Philipp Czerner, 2018
// See the file obst.cpp for license information.

// This file contains platform-specific code for Linux, responsible for initialising a window and 3D
// rendering context, as well as drawing a UI.

#include <X11/Xlib.h>
#include <GL/glx.h>
#include <time.h>
#include <locale.h>

//#include "obst.cpp"

#include <unistd.h>
#include <algorithm>
#include <cerrno>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <initializer_list>

#ifndef NDEBUG
#include <cassert>
#define assert_false assert(false)
#else
#define assert(x) (void)__builtin_expect(not (expr), 0)
#define assert_false __builtin_unreachable()
#endif

// Defer macro. Based on Jonathan Blow's code at https://pastebin.com/3YvWQa5c, although rewritten
// from scratch.
template <typename T>
struct Deferrer {
    T t;
    Deferrer(T const& t): t{t} {}
    ~Deferrer() { t(); }
};
struct Deferrer_helper {
    template <typename T>
    auto operator+ (T const& t) { return Deferrer<T> {t}; }
};
#define DEFER_NAME1(x, y) x##y
#define DEFER_NAME(x) DEFER_NAME1(_defer, x)
#define defer auto DEFER_NAME(__LINE__) = Deferrer_helper{} + [&]


// Standard integer types
using s64 = long long; // gcc and emcc (well, their shipped standard libraries) have different opinions about using long long or just long as 64-bit integer types. But for printf I just want to write one of them. Yay.
using u64 = unsigned long long;
//using s64 = std::int64_t;
//using u64 = std::uint64_t;
using s32 = std::int32_t;
using u32 = std::uint32_t;
using s16 = std::int16_t;
using u16 = std::uint16_t;
using s8 = std::int8_t;
using u8 = std::uint8_t;


#ifndef X_HAVE_UTF8_STRING
#error "There is no Xutf8LookupString! You are on an old version of X."
#endif

typedef GLXContext glXCreateContextAttribsARB_t(
    Display *dpy, GLXFBConfig config, GLXContext share_context, Bool direct, const int *attrib_list
);

void application_render() {
    glClearColor(0.f, 1.f, 0.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    glFlush();
}

double platform_now() {
    timespec t;
    clock_gettime(CLOCK_MONOTONIC_RAW, &t);
    return (double)t.tv_sec + (double)t.tv_nsec * 1e-9;
}

struct Key {
    enum Key_type: u8 {
        NONE, TEXT, SPECIAL
    };
    enum Key_special: u8 {
        INVALID, ESCAPE, RETURN, TAB, ARROW_L, ARROW_R, ARROW_D, ARROW_U,
        HOME, END, PAGE_UP, PAGE_DOWN,
        F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12
    };

    u8 type;
    union {
        u8 special;
        u8 text[15];
    };
};

//Key linux_get_event_key(XKeyEvent e) {
//    
//}

int main(int, char**) {
    // Do the OpenGL and X dance. I would recommend everyone to not read this code, if at all
    // possible, to preserve sanity. This should have been a single function call. If you must,
    // refer to the GLX 1.4 specification, the GLX_ARB_create_context extension, and the Xlib
    // specification to understand what all of this does.

    if (setlocale(LC_ALL, "") == nullptr) {
        fprintf(stderr, "Warning: Could not set default locale.\n");
    }
    
    // Create the display
    Display* display = XOpenDisplay(nullptr);
    if (not display) {
        fprintf(stderr, "Error: could not open display (is the DISPLAY environment variable set correctly?)\n");
        exit(101);
    }

    int screen = DefaultScreen(display);
    
    // Check for GLX 1.4 and extensions
    int glx_version[] = {1, 4};
    if (not glXQueryVersion(display, &glx_version[0], &glx_version[1])) {
        fprintf(stderr, "Error: glX version %d.%d not present\n", glx_version[0], glx_version[1]);
        exit(102);
    }

    char* gl_ext_present = (char*)glXQueryExtensionsString(display, screen);
    char* gl_ext_want[] = {(char*)"GLX_ARB_create_context"};
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
        None
    };
    GLXFBConfig* config = glXChooseFBConfig(display, screen, config_attribs, &config_size);
    defer { XFree(config); };
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

    glXCreateContextAttribsARB_t* glXCreateContextAttribsARB
        = (glXCreateContextAttribsARB_t*)glXGetProcAddress((u8*)"glXCreateContextAttribsARB");
    assert(glXCreateContextAttribsARB); // We already checked for the presence of the extension

    //glXDestroyContext(display, context_old);

    // Create an OpenGL 3.2 context
    int context_attribs[] = {
        GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
        GLX_CONTEXT_MINOR_VERSION_ARB, 2,
        None
    };
    GLXContext context = glXCreateContextAttribsARB(display, *config, 0, true, context_attribs);

    // Create the window
    XVisualInfo* visual = glXGetVisualFromFBConfig(display, *config);
    assert(visual);
    defer { XFree(visual); };

    XSetWindowAttributes window_attrs = {};
    window_attrs.colormap = XCreateColormap(display, DefaultRootWindow(display), visual->visual, AllocNone); // Apparently you need the colormap, else XCreateWindow gives a BadMatch error. No worries, this fact features prominently in the documentation and it was no bother at all.
    window_attrs.event_mask = ExposureMask | KeyPressMask | KeyReleaseMask;
    
    Window window = XCreateWindow(display, DefaultRootWindow(display),
        0, 0, DisplayWidth(display, screen), DisplayHeight(display, screen), 0,
    visual->depth, InputOutput, visual->visual, CWColormap | CWEventMask, &window_attrs); // We pass a type of InputOutput explicitly, as visual->c_class is an illegal value for some reason. A good reason, I hope.

    GLXWindow window_glx = glXCreateWindow(display, *config, window, nullptr);

    // Map the context to the window
    glXMakeContextCurrent(display, window_glx, window_glx, context); // This returns a bool, but I cannot find what it means in the spec, so just ignore it. The greatness continues.

    XMapWindow(display, window);
    
    int keypress_buffer_size = 16;
    char* keypress_buffer = (char*)malloc(keypress_buffer_size);
    defer { free(keypress_buffer); };

    while (true) {
        XEvent event;
        XNextEvent(display, &event);
        
        switch (event.type) {
        case Expose:
            application_render();
            glXSwapBuffers(display, window_glx);
            break;
        case KeyPress: {
            printf("KeyPress %d\n", event.xkey.keycode);
            KeySym keysym;
            int n = XLookupString(&event.xkey, keypress_buffer, keypress_buffer_size, &keysym, NULL);
            if (n < 0) {
                printf("Error!\n");
            } else {
                printf("result '%s' ", keypress_buffer);
                for (int i = 0; i < n; ++i) {
                    printf("%02x", (u8)keypress_buffer[i]);
                }
                printf("\n");
            }

        }
            break;
        case MappingNotify:
            if (event.xmapping.request == MappingModifier or event.xmapping.request == MappingKeyboard) {
                XRefreshKeyboardMapping(&event.xmapping);
            }
        default:
            break;
        }


        printf("%d\n", event.type);
    }
}


