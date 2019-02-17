
#include <emscripten/emscripten.h>
#include <emscripten/html5.h>
#include <GLES2/gl2.h>

#include "global.hpp"
#include "platform.hpp"
#include "obst.cpp"

// This is a dummy, so that our buildscript can grep for it and add the functions to the emcc
// command-line parameters
#define EM_EXPORT(x) x

// Display this text as an error message somewhere
EM_JS(void, _platform_ui_error_report, (char* msg), {
    document.getElementById("error-cont").textContent = UTF8ToString(msg);
    document.getElementById("error-hr").style.display = "block";
});
void platform_ui_error_report(Array_t<u8> msg) {
    assert(*msg.end() == 0);
    _platform_ui_error_report((char*)msg.begin());
}

// Reset the error display
EM_JS(void, _platform_ui_error_clear, (), {
    document.getElementById("error-cont").textContent = "";
    document.getElementById("error-hr").style.display = "none";
});
void platform_ui_error_clear () { _platform_ui_error_clear(); }

// @Cleanup: Replace abort() calls by something a little bit more useful.

// Called whenever the canvas resizes. This causes the internal viewport to adopt the new
// dimensions, regenerates the font to properly align the pixels, and redraws.
int _platform_resize_callback(int, const EmscriptenUiEvent*, void* user_data) {
    Webgl_context* context = (Webgl_context*)user_data;
    emscripten_get_element_css_size("canvas", &context->width, &context->height);
    emscripten_set_canvas_element_size("canvas", (int)context->width, (int)context->height);
    application_handle_resize();
    emscripten_resume_main_loop();
    return true;
}

// Render text into the currently selected TEXTURE_2D with the specified size. Writes the offsets
// into offsets
EM_JS(int, _platform_text_prepare, (int size, int w, float* offsets), {
    var canvas = document.createElement("canvas");
    canvas.width = w;
    canvas.height = w;
    var ctx = canvas.getContext("2d");
    ctx.fillStyle = "black";
    ctx.font = size + "px sans-serif";
    ctx.textAlign = "start";
    ctx.textBaseline = "top";
    
    // I would really like to use the advanced text measurement options here, but they are not yet
    // made available by Firefox and Chrome. This measures how high the 0 is, which I found better
    // for cross-browser consistency than just trusting the fonts to have similar offsets.
    ctx.clearRect(0, 0, size, size);
    var m = ctx.measureText("0");
    ctx.fillText("0", 0.5, 0.5);
    var i;
    var data = ctx.getImageData(0, 0, m.width, size);
    var actualTop = 0;
    var actualBot = 0;
    var greater_zero = /** @type {function(number):boolean} */ function(x) { return x > 0 && x < 255; };
    for (i = 0; i < size; i++) {
        var flag = data.data.slice(i*4*m.width, (i+1)*4*m.width).some(greater_zero);
        if (flag) {
            actualTop = i+2;
        }
        if (!flag && actualBot == i) {
            actualBot = i+1;
        }
    }

    ctx.clearRect(0, 0, w, w);
    
    var i;
    var x = 1;
    var y = 1;
    for (i = 0; i < 12; i++) {
        // We just need the digits, T and F as glyphs
        var s = i < 10 ? ""+i : i == 10 ? "F" : "T";
        var m = ctx.measureText(s);
        ctx.fillText(s, x+0.5, y+0.5);
        setValue(offsets+i*16,     x           /w, "float");
        setValue(offsets+i*16+ 4, (y+actualBot)/w, "float");
        setValue(offsets+i*16+ 8, (x+m.width)  /w, "float");
        setValue(offsets+i*16+12, (y+actualTop)/w, "float");
        x = x + m.width + 1;
        if (x + size >= w) {
            x = 1;
            y = y + size + 1;
        }
    }
    var gl = document.getElementById("canvas").getContext("webgl");
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, canvas);
    return actualTop - actualBot;
});
int platform_text_prepare(int size, int w, float* offsets) {
    return _platform_text_prepare(size, w, offsets);
}
    
// Query the value of an input element
EM_JS(char*, _platform_get_value_js, (char* element), {
    var s = document.getElementById(UTF8ToString(element)).value;
    var l = lengthBytesUTF8(s)+1;
    var s_ = _malloc(l);
    stringToUTF8(s, s_, l+1);
    return s_;
});
EM_JS(char*, _platform_get_radio_value_js, (char* element), {
    var s = document.querySelector('input[name=' + UTF8ToString(element) + ']:checked').value;
    var l = lengthBytesUTF8(s)+1;
    var s_ = _malloc(l);
    stringToUTF8(s, s_, l+1);
    return s_;
})
Array_t<u8> platform_ui_get_value(u8 elem) {
    assert(elem < Ui_elem::NAME_COUNT);
    char* s;
    if (elem != Ui_elem::OPERATION) {
        // Normal button
        s = _platform_get_value_js(Ui_elem::name[elem]);
    } else {
        // Radiobutton
        s = _platform_get_radio_value_js(Ui_elem::name[elem]);
    }
    return {(u8*)s, strlen(s)};
}

// bddinfo is the hover text telling you what a node is about

EM_JS(void, _platform_ui_bddinfo_hide, (), {
    document.getElementById('cont-bddinfo').style.display = "none";
})
void platform_ui_bddinfo_hide() { _platform_ui_bddinfo_hide(); }

EM_JS(void, _platform_ui_bddinfo_show_js, (float x, float y, char* text, int right, int bottom), {
    var s = UTF8ToString(text);
    var e = document.getElementById('cont-bddinfo');
    e.style.display = "";
    if (right) {
        e.style.right = x + "px";
        e.style.left = "";
    } else {
        e.style.right = "";
        e.style.left = x + "px";
    }
    if (bottom) {
        e.style.bottom = y + "px";
        e.style.top = "";
    } else {
        e.style.bottom = "";
        e.style.top = y + "px";
    }
    e.innerHTML = s;
})

// Display the text at the specified position. This is the platform-level call for ui_bddinfo_show.
void platform_ui_bddinfo_show(float x, float y, Array_t<u8> text) {
    float pd = global_context.draw_param.node_radius * global_context.scale * 1.35;
    float px = (x - global_context.origin_x) * global_context.scale;
    float py = (y - global_context.origin_y) * global_context.scale;
    int right = 0;
    int bottom = 1;
    
    // Try to draw the box inside the canvas
    if (px + pd + 300.f >= global_context.width) {
        px = global_context.width - px;
        right = 1;
    }
    if (py + 200.f >= global_context.height) {
        py = global_context.height - py;
        bottom = 0;
    }
    _platform_ui_bddinfo_show_js(px + pd, py, (char*)text.data, right, bottom);
}

EM_JS(void, _platform_ui_context_set_js, (char* s, int frame, int frame_max), {
    document.getElementById("context-cont").innerHTML = UTF8ToString(s);
    document.getElementById("frame").textContent = frame + "/" + frame_max;
});

// Update the HTML displaying the context
void platform_ui_context_set(Array_t<u8> text, int frame, int frame_max) {
    assert(*text.end() == 0);
    _platform_ui_context_set_js((char*)text.begin(), frame, frame_max);
}

double platform_now() {
    return emscripten_get_now() * 0.001;
}

EM_BOOL _platform_ui_mouse_move(int, EmscriptenMouseEvent const* event, void*) {
    float x = (float)event->canvasX / global_context.scale + global_context.origin_x;
    float y = (global_context.height - (float)event->canvasY) / global_context.scale + global_context.origin_y;
    ui_mouse_move(x, y);
    return false;
}

// Returns the position of the mouse in world coordinates, NOT pixels.
void platform_mouse_position(float* out_x, float* out_y) {
    EmscriptenMouseEvent event;
    emscripten_get_mouse_status(&event);
    float x = (float)event.canvasX / global_context.scale + global_context.origin_x;
    float y = (global_context.height - (float)event.canvasY) / global_context.scale + global_context.origin_y;
    if (out_x) *out_x = x;
    if (out_y) *out_y = y;
}

// Enable the right elements depending on the operation selected.
extern "C" void EM_EXPORT(_platform_ui_button_opr) () {
    Array_t<u8> op_str = platform_ui_get_value(Ui_elem::OPERATION);
    assert(op_str.size == 1);

    if (op_str[0] == 'u') {
        EM_ASM(
            document.getElementById("b_op").textContent = "Calculate union";
            document.getElementById("op_second").className = "init-enabled";
            document.getElementById("op_node1").disabled = false;
        );
    } else if (op_str[0] == 'i') {
        EM_ASM(
            document.getElementById("b_op").textContent = "Calculate intersection";
            document.getElementById("op_second").className = "init-enabled";
            document.getElementById("op_node1").disabled = false;
        );
    } else if (op_str[0] == 'c') {
        EM_ASM(
            document.getElementById("b_op").textContent = "Calculate complement";
            document.getElementById("op_second").className = "init-disabled";
            document.getElementById("op_node1").disabled = true;
        );
    } else {
        assert_false;
    }
}

// Show/hide the helptext
void platform_ui_button_help () {
    global_ui.is_helptext_visible ^= 1;
    if (global_ui.is_helptext_visible) {
        EM_ASM(
            document.getElementById("cont-overlay").style.display = "";
            document.getElementById("b_help").textContent = "Hide help";
        );
    } else {
        EM_ASM(
            document.getElementById("cont-overlay").style.display = "none";
            document.getElementById("b_help").textContent = "Show help";
        );
    }
}

// Simply dispatch to the respective ui_button_* procedures. Why? The extern "C" declaration is
// emscripten-specific and does not belong in obst.cpp .
extern "C" void EM_EXPORT(_platform_ui_button_help) () {
    platform_ui_button_help();
}
extern "C" void EM_EXPORT(_platform_ui_button_op) () {
    ui_button_op();
}
extern "C" void EM_EXPORT(_platform_ui_button_create) () {
    ui_button_create();
}
extern "C" void EM_EXPORT(_platform_ui_button_removeall) () {
    ui_button_removeall();
}
extern "C" void EM_EXPORT(_platform_ui_button_move) (float diff) {
    ui_button_move(diff);
}

// Enable the buttons that perform set operations. bdd is used to populate fields with valid
// input, so that the user can simply click on the "Calculate X" button and see something sensible.
void platform_operations_enable(u32 bdd) {
    if (bdd > 1) {
        EM_ASM({
            document.getElementById("op_node0").value = $0 > 1 ? $0 : "T";
            document.getElementById("op_node1").value = $1 > 1 ? $1 : "T";
        }, global_store.bdd_data[bdd].child0, global_store.bdd_data[bdd].child1);
    }

    EM_ASM(
        document.getElementById("op-cont"   ).className = "init-enabled";
        document.getElementById("reset-cont").className = "init-enabled";
        document.getElementById("frame-cont").className = "init-enabled";
        document.getElementById("b_op").disabled = false;
        document.getElementById("op_u").disabled = false;
        document.getElementById("op_i").disabled = false;
        document.getElementById("op_c").disabled = false;
        document.getElementById("op_node0").disabled = false;
        document.getElementById("op_node1").disabled = false;
        document.getElementById("b_removeall").disabled = false;
        document.getElementById("b_prev").disabled = false;
        document.getElementById("b_next").disabled = false;
    );
    _platform_ui_button_opr();
}

void platform_operations_disable() {
    EM_ASM({
        document.getElementById("op-cont"   ).className = "init-disabled";
        document.getElementById("reset-cont").className = "init-disabled";
        document.getElementById("frame-cont").className = "init-disabled";
        document.getElementById("b_op").disabled = true;
        document.getElementById("b_op").textContent = "Calculate union";
        document.getElementById("op_u").disabled = true;
        document.getElementById("op_i").disabled = true;
        document.getElementById("op_c").disabled = true;
        document.getElementById("op_node0").disabled = true;
        document.getElementById("op_node1").disabled = true;
        document.getElementById("b_removeall").disabled = true;
        document.getElementById("b_prev").disabled = true;
        document.getElementById("b_next").disabled = true;
        document.getElementById("loadtext").style.display = "none";
        document.getElementById("helptext").style.display = "";
    });
    
    // Hide the helptext. Note that ui_button_help toggles the status.
    global_ui.is_helptext_visible = true;
    _platform_ui_button_help();
}

EM_BOOL _platform_ui_key_press(int, EmscriptenKeyboardEvent const* event, void*) {
    // This is a bit hacky. If an input element is selected, we ignore keypresses to avoid doing
    // frames of the animation if the user just wants to move the cursor in a text entry.
    if (global_ui.focus_flags) return false;

#define CHECK_KEY(x, y)                         \
    if (strncmp(event->key, (x), 32) == 0) {    \
        key = Key::create_special(Key::y);      \
    } else

    Key key;
    
    CHECK_KEY("ArrowRight", ARROW_R)
    CHECK_KEY("ArrowLeft",  ARROW_L)
    CHECK_KEY("ArrowUp",    ARROW_U)
    CHECK_KEY("ArrowDown",  ARROW_D)
    CHECK_KEY("Home", HOME)
    CHECK_KEY("End", END)
    CHECK_KEY("PageUp", PAGE_U)
    CHECK_KEY("PageDown", PAGE_D)
    CHECK_KEY("F1", F1)
    CHECK_KEY("F2", F2)
    CHECK_KEY("F3", F3)
    CHECK_KEY("F4", F4)
    CHECK_KEY("F5", F5)
    CHECK_KEY("F6", F6)
    CHECK_KEY("F7", F7)
    CHECK_KEY("F8", F8)
    CHECK_KEY("F9", F9)
    CHECK_KEY("F10", F10)
    CHECK_KEY("F11", F11)
    CHECK_KEY("F12", F12)
    /*else*/ { return false; }

    return ui_key_press(key);
}

// We want to collect information on whether the following elements are focused or not. If any of
// them are, we ignore keypresses.
char const* focusable_ids[] = {"create_nums", "create_base", "create_bits", "op_node0", "op_node1", 0};
EM_BOOL _platform_ui_focus(int event_type, EmscriptenFocusEvent const* event, void*) {
    u64 id;
    for (id = 0; focusable_ids[id]; ++id) {
        if (strncmp(event->id, focusable_ids[id], 32) == 0) break;
    }
    if (not focusable_ids[id]) return false;
    
    u64 focused;
    if (event_type == EMSCRIPTEN_EVENT_BLUR) {
        focused = 0;
    } else if (event_type == EMSCRIPTEN_EVENT_FOCUS) {
        focused = 1;
    } else {
        return false;
    }
    global_ui.focus_flags ^= (global_ui.focus_flags ^ (focused << id)) & 1ull << id;
    return false;
}

// Change the status of the main loop callback. If is_active is true, the main loop will be called
// every frame. (Or so.)
void platform_main_loop_active(bool is_active) {
    if (is_active) {
        emscripten_resume_main_loop();
    } else {
        emscripten_pause_main_loop();
    }
}

void _platform_init_context(Webgl_context* context) {
    assert(context);
    
    EMSCRIPTEN_WEBGL_CONTEXT_HANDLE ctx;
    EmscriptenWebGLContextAttributes attrs;
    emscripten_webgl_init_context_attributes(&attrs);
    attrs.alpha = false;
    attrs.depth = true;
    attrs.antialias = true; // BDDs and edges do their own AA, but for arrows this still is convenient
    attrs.majorVersion = 1;
    
    ctx = emscripten_webgl_create_context(0, &attrs);
    if (not ctx) {
        ui_error_report("Error while creating WebGL context.");
        abort();
    }
    emscripten_webgl_make_context_current(ctx);
    emscripten_set_resize_callback(nullptr, context, false, _platform_resize_callback);
    _platform_resize_callback(0, 0, context); // Easy way to set the initial values correctly

    webgl_init(context);
}

// Entry point. Set up the callbacks and do initialisation.
int EM_EXPORT(main) () {
    emscripten_set_main_loop(&application_render, 0, false);
    emscripten_pause_main_loop();

    // Chrome does not issue keypress events for navigation keys (e.g. arrow keys, page up). So we
    // use keydown instead, which works basically the same.
    emscripten_set_keydown_callback(nullptr, nullptr, false, &_platform_ui_key_press);

    for (u64 id = 0; focusable_ids[id]; ++id) {
        emscripten_set_blur_callback (focusable_ids[id], nullptr, false, &_platform_ui_focus);
        emscripten_set_focus_callback(focusable_ids[id], nullptr, false, &_platform_ui_focus);
    }

    emscripten_set_mousemove_callback("canvas", nullptr, false, &_platform_ui_mouse_move);

    _platform_init_context(&global_context);
    application_main();
    
    return 0;
}
