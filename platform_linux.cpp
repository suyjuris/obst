
// Written by Philipp Czerner, 2018
// See the file obst.cpp for license information.

// This file contains platform-specific code for Linux, responsible for initialising a window and 3D
// rendering context, as well as drawing a UI.

#include <X11/Xlib.h>
#include <X11/keysym.h>
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


template <typename T_>
struct Array_t {
    using T = T_;
    T* data = nullptr;
    s64 size = 0;

    T& operator[] (int pos) {
		assert(0 <= pos and pos < size);
		return data[pos];
	}

    // See the E macro below.
    //T& dbg(int pos, int line) {
    //    if (not (0 <= pos and pos < size)) {
    //        printf("line: %d\n", line);
    //        abort();
    //    }
	//	return data[pos];
	//}

    T* begin() { return data; }
	T* end()   { return data + size; }
};
template <typename T_>
struct Array_dyn: public Array_t<T_> {
    using T = T_;
    s64 capacity;

    Array_dyn(T* data = nullptr, s64 size = 0, s64 capacity = 0):
        Array_t<T>::Array_t{data, size},
        capacity{capacity} {}
    
    explicit Array_dyn(Array_t<T> arr) :
        Array_t<T>::Array_t{arr.data, 0},
        capacity{arr.size} {}
    
    T& operator[] (int pos) {
		assert(0 <= pos and pos < Array_t<T>::size);
		return Array_t<T>::data[pos];
	}

    // See the E macro below.
    //T& dbg (int pos, int line) {
    //    if (0 <= pos and pos < Array_t<T>::size) {
    //        return Array_t<T>::data[pos];
    //    } else {
    //        printf("out of bounds, index %d size %lld, line %d\n", pos, Array_t<T>::size, line);
    //        abort();
    //    }
    //}

    T* begin() const { return (T*)Array_t<T>::data; }
	T* end()   const { return (T*)(Array_t<T>::data + Array_t<T>::size); }
};

// Allocation. Returns zeroed memory.
template <typename T>
Array_t<T> array_create(s64 size) {
    return {(T*)calloc(sizeof(T), size), size};
}

// Take some bytes from an already existing memory location. Advance p by the number of bytes used.
template <typename T>
Array_t<T> array_create_from(u8** p, s64 size) {
    Array_t<T> result = {(T*)*p, size};
    *p += sizeof(T) * size;
    return result;
}

// Free the memory, re-initialise the array.
template <typename T>
void array_free(Array_t<T>* arr) {
    assert(arr);
    free(arr->data);
    arr->data = nullptr;
    arr->size = 0;
}
template <typename T>
void array_free(Array_dyn<T>* arr) {
    assert(arr);
    free(arr->data);
    arr->data = nullptr;
    arr->size = 0;
    arr->capacity = 0;
}

// Ensure that there is space for at least count elements.
template <typename T>
void array_reserve(Array_dyn<T>* into, s64 count) {
    if (count > into->capacity) {
        s64 capacity_new = 2 * into->capacity;
        if (capacity_new < count) {
            capacity_new = count;
        }
        if (into->data) {
            into->data = (T*)std::realloc(into->data, capacity_new * sizeof(T));
        } else {
            into->data = (T*)std::malloc(capacity_new * sizeof(T));
        }
        assert(into->data);
        into->capacity = capacity_new;
        assert(into->data);
    }
}

// Set the array's size to count, reallocate if necessary.
template <typename T>
void array_resize(Array_t<T>* arr, s64 count) {
    arr->data = (T*)realloc(arr->data, count * sizeof(T));
    if (arr->size < count) {
        memset(arr->data + arr->size, 0, (count - arr->size) * sizeof(T));
    }
    arr->size = count;
}
template <typename T>
void array_resize(Array_dyn<T>* arr, s64 count) {
    array_reserve(arr, count);
    arr->size = count;
}

// Add element to the end of an array, reallocate if necessary.
template <typename T>
void array_push_back(Array_dyn<T>* into, T elem) {
    array_reserve(into, into->size + 1);
    ++into->size;
    into->data[into->size-1] = elem;
}

// Return an array that represents the sub-range [start, end). start == end is fine (but the result
// will use a nullptr).
template <typename T>
Array_t<T> array_subarray(Array_t<T> arr, s64 start, s64 end) {
    assert(0 <= start and start <= arr.size);
    assert(0 <= end   and end   <= arr.size);
    assert(start <= end);
    if (start == end)
        return {nullptr, 0};
    else
        return {arr.data + start, end - start};
}


#ifndef X_HAVE_UTF8_STRING
#error "There is no Xutf8LookupString! You are on an old version of X."
#endif

typedef GLXContext glXCreateContextAttribsARB_t(
    Display *dpy, GLXFBConfig config, GLXContext share_context, Bool direct, const int *attrib_list
);

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
        INVALID, ESCAPE, ARROW_L, ARROW_R, ARROW_D, ARROW_U,
        HOME, END, PAGE_U, PAGE_D,
        C_COPY, C_PASTE, C_SELECTALL, C_UNDO, C_REDO, C_QUIT, C_SAVE,
        F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
        SPECIAL_COUNT
    };
    static constexpr char const* key_special_names[] = {
        "invalid", "escape", "arrow_l", "arrow_r", "arrow_d", "arrow_u",
        "home", "end", "page_u", "page_d",
        "c_copy", "c_paste", "c_selectall", "c_undo", "c_redo", "c_quit", "c_save",
        "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"
    };

    u8 type = Key::NONE;
    union {
        u8 text[15];
        u8 special;
    };

    static Key create_text(Array_t<char> text_) {
        assert(text_.size+1 < (s64)sizeof(Key::text));
        Key result;
        result.type = Key::TEXT;
        memcpy(result.text, text_.data, text_.size);
        result.text[text_.size] = 0;
        return result;
    }
    static Key create_special(u8 special) {
        assert(special != INVALID and special < SPECIAL_COUNT);
        Key result;
        result.type = Key::SPECIAL;
        result.special = special;
        return result;
    }
};

constexpr char const* Key::key_special_names[];

void print_key(Key key) {
    if (key.type == Key::NONE) {
        printf("{NONE}");
    } else if (key.type == Key::TEXT) {
        printf("{\"%s\" %02x}", key.text, (u8)key.text[0]);
    } else if (key.type == Key::SPECIAL) {
        assert(0 <= key.special and key.special < Key::SPECIAL_COUNT);
        printf("{%s}", Key::key_special_names[key.special]);
    } else {
        assert(false);
    }
}

struct Platform_state {
    bool is_sleeping = false;
    Array_dyn<Key> input_queue;
};

void application_render() {
    glClearColor(0.f, 1.f, 0.f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    glFlush();
}

void application_update(Platform_state* state) {
    for (Key i: state->input_queue) {
        print_key(i);
        printf("\n");
        if (i.type == Key::SPECIAL and i.special == Key::C_QUIT) {
            exit(0);
        }
    }
    state->input_queue.size = 0;
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

    linux_set_wm_prop(display, window, "WM_CLASS", instance);
}

int main(int argc, char** argv) {
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
    //defer { XFree(visual); };

    XSetWindowAttributes window_attrs = {};
    window_attrs.colormap = XCreateColormap(display, DefaultRootWindow(display), visual->visual, AllocNone); // Apparently you need the colormap, else XCreateWindow gives a BadMatch error. No worries, this fact features prominently in the documentation and it was no bother at all.
    window_attrs.event_mask = ExposureMask | KeyPressMask | KeyReleaseMask;
    
    Window window = XCreateWindow(display, DefaultRootWindow(display),
        0, 0, 1300, 800, 0,
    visual->depth, InputOutput, visual->visual, CWColormap | CWEventMask, &window_attrs); // We pass a type of InputOutput explicitly, as visual->c_class is an illegal value for some reason. A good reason, I hope.

    // Initialise window properties
    linux_set_wm_prop(display, window, "WM_NAME", "obst - Binary Decision Diagrams");
    linux_set_wm_prop(display, window, "WM_ICON_NAME", "obst");
    linux_set_wm_class(display, window, argc, argv);
    
    
    GLXWindow window_glx = glXCreateWindow(display, *config, window, nullptr);

    // Map the context to the window
    glXMakeContextCurrent(display, window_glx, window_glx, context); // This returns a bool, but I cannot find what it means in the spec, so just ignore it. The greatness continues.

    XMapWindow(display, window);
    
    Platform_state state;
    //defer { array_free(&state.input_queue); };

    while (true) {
        XEvent event;
        XNextEvent(display, &event);
        
        switch (event.type) {
        case Expose:
            application_render();
            glXSwapBuffers(display, window_glx);
            break;
        case KeyPress:
            linux_get_event_key(&state.input_queue, event.xkey);
            application_update(&state);
            break;
        case MappingNotify:
            if (event.xmapping.request == MappingModifier or event.xmapping.request == MappingKeyboard) {
                XRefreshKeyboardMapping(&event.xmapping);
            }
        default:
            break;
        }
    }

    // Memory freed by OS
}


