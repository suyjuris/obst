
#include <emscripten/emscripten.h>
#include <emscripten/html5.h>
#include <GLES2/gl2.h>

#define OBST_PLATFORM_EMSCRIPTEN

#include "global.hpp"
#include "platform.hpp"
#include "obst.cpp"

// This is a dummy, so that our build script can grep for it and add the functions to the emcc
// command-line parameters
#define OBST_EM_EXPORT(x) x

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

struct Emscripten_state {
    Array_dyn<u8> fmt_buf;
    u64 fmt_flags;

    Array_t<u8> prep_buf;
    s64 prep_texture_size = 512;
};
Emscripten_state global_emscripten;

void platform_fmt_init() {
    auto* state = &global_emscripten;
    state->fmt_buf.size = 0;
    state->fmt_flags = 0;
}

void platform_fmt_begin(u64 flags) {
    auto* state = &global_emscripten;
    
    state->fmt_flags |= flags;

    if (flags & Text_fmt::PARAGRAPH) {
        array_printf(&state->fmt_buf, "<p>");
    } else if (flags & Text_fmt::PARAGRAPH_CLOSE) {
        array_printf(&state->fmt_buf, "<p class=\"close\">");
    }

    if (flags & Text_fmt::BOLD) {
        array_printf(&state->fmt_buf, "<b>");
    }
    if (flags & Text_fmt::SANS) {
        array_printf(&state->fmt_buf, "<span style=\"font-family: Dejavu Sans, sans\">");
    }
    if (flags & Text_fmt::ITALICS) {
        array_printf(&state->fmt_buf, "<i>");
    }
    if (flags & Text_fmt::RED) {
        array_printf(&state->fmt_buf, "<span class=\"nicered\">");
    }
}

void platform_fmt_end(u64 flags) {
    auto* state = &global_emscripten;
        
    if (flags & Text_fmt::PARAGRAPH) {
        array_printf(&state->fmt_buf, "</p>");
    } else if (flags & Text_fmt::PARAGRAPH_CLOSE) {
        array_printf(&state->fmt_buf, "</p>");
    } else if (flags & Text_fmt::NEWLINE) {
        array_printf(&state->fmt_buf, "<br>");
    }

    if (flags & Text_fmt::BOLD) {
        array_printf(&state->fmt_buf, "</b>");
    }
    if (flags & Text_fmt::SANS) {
        array_printf(&state->fmt_buf, "</span>");
    }
    if (flags & Text_fmt::ITALICS) {
        array_printf(&state->fmt_buf, "</i>");
    }
    if (flags & Text_fmt::RED) {
        array_printf(&state->fmt_buf, "</span>");
    }
    
    state->fmt_flags &= ~flags;
}

void platform_fmt_text(u64 flags, Array_t<u8> text) {
    auto* state = &global_emscripten;
    
    platform_fmt_begin(flags);
    array_append(&state->fmt_buf, text);

    if (text.size and not (flags & (Text_fmt::NOSPACE | Text_fmt::STICKY))) {
        array_printf(&state->fmt_buf, " ");
    }
    platform_fmt_end(flags);
}

char* _platform_get_elem(s64 slot) {
    switch (slot) {
    case Text_fmt::SLOT_CONTEXT:       return (char*)"context-cont";
    case Text_fmt::SLOT_CONTEXT_FRAME: return (char*)"frame";
    case Text_fmt::SLOT_BDDINFO:       return (char*)"cont-bddinfo";
    case Text_fmt::SLOT_HELPTEXT:      return (char*)"helptext";
    case Text_fmt::SLOT_ERRORINFO:     return (char*)"error-cont";
    default: assert(false); return nullptr;
    };
}

void platform_fmt_store(s64 slot) {
    auto* state = &global_emscripten;

    array_push_back(&state->fmt_buf, (u8)0);
    --state->fmt_buf.size;

    EM_ASM_({
        document.getElementById(UTF8ToString($0)).innerHTML = UTF8ToString($1);
    }, _platform_get_elem(slot), state->fmt_buf.data);

    if (slot == Text_fmt::SLOT_ERRORINFO) {
        EM_ASM_({
            document.getElementById("error-hr").style.display = UTF8ToString($0);
        }, state->fmt_buf.size > 0 ? "block" : "none");
    }
}

EM_JS(void, _platform_ui_cursor_set, (char* name, int pos), {
    var e = document.getElementById(UTF8ToString(name));
    e.focus();
    e.setSelectionRange(pos, pos);
})

void platform_ui_cursor_set(u8 elem, s64, s64, s64, s64 cursor_char) {
    _platform_ui_cursor_set(_platform_get_elem(elem), cursor_char);
}

// Called whenever the canvas resizes. This causes the internal viewport to adopt the new
// dimensions, regenerates the font to properly align the pixels, and redraws.
int _platform_resize_callback(int, const EmscriptenUiEvent*, void* user_data) {
    Webgl_context* context = (Webgl_context*)user_data;
    emscripten_get_element_css_size("canvas", &context->width, &context->height);
    emscripten_set_canvas_element_size("canvas", (int)context->width, (int)context->height);
    global_context.screen_w = (s64)context->width;
    global_context.screen_h = (s64)context->height;

    context->canvas_x = 0; // In the emscripten environment there is only the canvas to draw on
    context->canvas_y = 0;
    glViewport(0.0, 0.0, context->width, context->height);
    application_handle_resize();
    emscripten_resume_main_loop();
    return true;
}

EM_JS(void, _platform_text_prepare_init, (int canvas_size, float font_size), {
    //var canvas = document.createElement("canvas");
    var canvas = document.getElementById("canvas2");
    Module.text_prepare_canvas = canvas;
    canvas.width = canvas_size;
    canvas.height = canvas_size;
    var ctx = canvas.getContext("2d");

    ctx.font = font_size + "px Dejavu Sans, sans-serif";
    ctx.textAlign = "start";
    ctx.textBaseline = "top";
    ctx.fillStyle = "black";

    ctx.clearRect(0, 0, canvas_size, canvas_size);
});

EM_JS(void, _platform_text_prepare_measure, (char* c, bool italics, float size, int* out_x0, int* out_y0, int* out_x1, int* out_y1), {
    var canvas = Module.text_prepare_canvas;
    var ctx = canvas.getContext("2d");
    var is_filled = /** @type {function(number):boolean} */ function(x) { return x > 0 && x < 255; };

    var w = size * 3;
    ctx.clearRect(0, 0, w, w);
    
    ctx.fillStyle = "grey";
    if (italics) {
        ctx.font = "italic " + size + "px Dejavu Serif, serif";
    } else {
        ctx.font = size + "px Dejavu Sans, sans-serif";
    }
    
    var s = UTF8ToString(c);
    ctx.fillText(s, size+0.5, size+0.5);
    
    var x0 = -1;
    var x1 = -1;
    var y0 = -1;
    var y1 = -1;
    
    var i;
    for (i = 0; i < w; i++) {
        var flag = ctx.getImageData(i, 0, 1, w).data.some(is_filled);
        if (flag && x0 == -1) { x0 = i; }
        if (flag && x0 != -1) { x1 = i+1; }
    }
    for (i = 0; i < w; i++) {
        var flag = ctx.getImageData(0, i, w, 1).data.some(is_filled);
        if (flag && y0 == -1) { y0 = i; }
        if (flag && y0 != -1) { y1 = i+1; }
    }

    if (out_x0) setValue(out_x0, x0 - size, 'i32');
    if (out_y0) setValue(out_y0, y0 - size, 'i32');
    if (out_x1) setValue(out_x1, x1 - size, 'i32');
    if (out_y1) setValue(out_y1, y1 - size, 'i32');
    
    ctx.clearRect(0, 0, w, w);
});


EM_JS(int, _platform_text_prepare_draw, (char* c, bool italics, float size, int x, int y, int* out_advance), {
    var canvas = Module.text_prepare_canvas;
    var ctx = canvas.getContext("2d");

    ctx.fillStyle = "black";
    if (italics) {
        ctx.font = "italic " + size + "px Dejavu Serif, serif";
    } else {
        ctx.font = size + "px Dejavu Sans, sans-serif";
    }
    
    var s = UTF8ToString(c);
    var advance = ctx.measureText(s).width;
    //ctx.fillStyle = "#55555555";
    //ctx.fillRect(x, y, advance, size);
    ctx.fillStyle = "black";
    ctx.fillText(s, x+0.5, y+0.5);
    setValue(out_advance, advance, 'i32');
});

bool _platform_text_prepare_helper(int texture_size, int size, float small_frac, Array_t<Text_box>* offsets, float* linoff_out, float* ascent_out) {
    _platform_text_prepare_init(texture_size, size);

    int linoff, ascent;
    _platform_text_prepare_measure("E", false, size, nullptr, &linoff, nullptr, &ascent);
    if (linoff_out) *linoff_out = (float)linoff;
    if (ascent_out) *ascent_out = (float)ascent;

    struct BB { int x0, y0, x1, y1; };
    array_resize(&global_emscripten.prep_buf, offsets->size * sizeof(BB));
    Array_t<BB> bbs = {(BB*)global_emscripten.prep_buf.data, offsets->size};
    
    for (s64 i = 0; i < offsets->size; ++i) {
        u8 c = webgl_bddlabel_index_char(i);
        bool italicized;
        auto arr = webgl_bddlabel_index_utf8(i, nullptr, &italicized);
        bool small = c & 128;

        int font_size = size;
        if (small) font_size *= small_frac;

        _platform_text_prepare_measure((char*)arr.data, italicized, font_size, &bbs[i].x0, &bbs[i].y0, &bbs[i].x1, &bbs[i].y1);
    }
    
    int x = 1;
    int y = 1;
    int yh = 0;
    for (s64 i = 0; i < offsets->size; ++i) {
        u8 c = webgl_bddlabel_index_char(i);
        bool italicized;
        auto arr = webgl_bddlabel_index_utf8(i, nullptr, &italicized);
        bool small = c & 128;

        int font_size = size;
        if (small) font_size *= small_frac;

        if (x + bbs[i].x1-bbs[i].x0 >= texture_size) {
            x = 1;
            y += yh + 1;
            yh = 0;
        }
        if (y + bbs[i].y1-bbs[i].y0 >= texture_size) {
            return false;
        }
        
        int advance;
        _platform_text_prepare_draw((char*)arr.data, italicized and not small, font_size, x-bbs[i].x0, y-bbs[i].y0, &advance);
        int ascent_i = small ? (int)std::round((float)ascent * small_frac) : ascent;

        Text_box box;
        box.x0 = (float) bbs[i].x0;
        box.y0 = (float)(bbs[i].y0 - ascent_i);
        box.x1 = (float) bbs[i].x1;
        box.y1 = (float)(bbs[i].y1 - ascent_i);
        box.s0 = (float)(x) / (float)texture_size;
        box.t0 = (float)(y) / (float)texture_size;
        box.s1 = (float)(x + bbs[i].x1-bbs[i].x0) / (float)texture_size;
        box.t1 = (float)(y + bbs[i].y1-bbs[i].y0) / (float)texture_size;
        box.advance = (float)advance;
        box.font = -1;
        (*offsets)[i] = box;
        
        yh = std::max(yh, bbs[i].y1 - bbs[i].y0);
        x += bbs[i].x1 - bbs[i].x0 + 1;
    }

    EM_ASM_({
        var gl = document.getElementById("canvas").getContext("webgl");
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.ALPHA, gl.ALPHA, gl.UNSIGNED_BYTE, Module.text_prepare_canvas);
    });
    
    return true;
}

void platform_text_prepare(int size, float small_frac, Array_t<Text_box>* offsets, float* linoff_out, float* ascent_out) {
    while (true) {
        bool flag = _platform_text_prepare_helper(
            global_emscripten.prep_texture_size, size, small_frac, offsets, linoff_out, ascent_out
        );
        if (flag) break;
        global_emscripten.prep_texture_size *= 2;
    }
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
Array_t<u8> platform_ui_value_get(u8 elem) {
    assert(elem < Ui_elem::NAME_COUNT);
    char* s;
    if (elem == Ui_elem::OPERATION or elem == Ui_elem::CREATE_TYPE) {
        // Radiobutton
        s = _platform_get_radio_value_js(Ui_elem::name[elem]);
    } else {
        // Normal button
        s = _platform_get_value_js(Ui_elem::name[elem]);
    }
    return {(u8*)s, strlen(s)};
}
void platform_ui_value_free(Array_t<u8> data) {
    array_free(&data);
}

// bddinfo is the hover text telling you what a node is about

EM_JS(void, _platform_ui_bddinfo_hide, (), {
    document.getElementById('cont-bddinfo').style.display = "none";
})
void platform_ui_bddinfo_hide() { _platform_ui_bddinfo_hide(); }

EM_JS(void, _platform_ui_bddinfo_show_js, (float x, float y, int right, int bottom), {
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
})

// Display the text at the specified position. This is the platform-level call for ui_bddinfo_show.
void platform_ui_bddinfo_show(float x, float y, float pad) {
    float px = (x - global_context.origin_x) * global_context.scale;
    float py = (y - global_context.origin_y) * global_context.scale;
    float pd = pad * global_context.scale;
    
    bool right = false;
    bool bottom = true;

    // Try to draw the box inside the canvas
    // @Cleanup bddinfo width
    if (px + pd + 300.f >= global_context.width) {
        px = global_context.width - px;
        right = true;
    }
    if (py + 200.f >= global_context.height) {
        py = global_context.height - py;
        bottom = false;
    }

    _platform_ui_bddinfo_show_js(px + pd, py, (int)right, (int)bottom);
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
    float y = (global_context.height-1 - (float)event->canvasY) / global_context.scale + global_context.origin_y;
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
extern "C" void OBST_EM_EXPORT(_platform_ui_button_opr) () {
    Array_t<u8> op_str = platform_ui_value_get(Ui_elem::OPERATION);
    defer { platform_ui_value_free(op_str); };
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

extern "C" void OBST_EM_EXPORT(_platform_ui_button_typer) () {
    Array_t<u8> type_str = platform_ui_value_get(Ui_elem::CREATE_TYPE);
    defer { platform_ui_value_free(type_str); };
    assert(type_str.size == 1);

    if (type_str[0] == 'n') {
        EM_ASM(
            var a = document.getElementById("create_nums");
            var b = document.getElementById("create_form");
            a.style.display = "";
            b.style.display = "none";
            a.style.height = b.style.height;
            document.getElementById("create_base").disabled = false;
            document.getElementById("create_base-cont").style.color = "";
            document.getElementById("create_order-cont").textContent = "Bit order:";
            document.getElementById("create_bits").style.display = "";
            document.getElementById("create_vars").style.display = "none";
        );
    } else if (type_str[0] == 'f') {
        EM_ASM(
            var a = document.getElementById("create_form");
            var b = document.getElementById("create_nums");
            a.style.display = "";
            b.style.display = "none";
            a.style.height = b.style.height;
            document.getElementById("create_base").disabled = true;
            document.getElementById("create_base-cont").style.color = "rgba(0,0,0,0.5)";
            document.getElementById("create_order-cont").textContent = "Variable order:";
            document.getElementById("create_vars").style.display = "";
            document.getElementById("create_bits").style.display = "none";
        );
    } else {
        assert_false;
    }
}

// Show/hide the helptext
void platform_ui_button_help() {
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

bool platform_ui_help_active() {
    return global_ui.is_helptext_visible;
}

// Simply dispatch to the respective ui_button_* procedures. Why? The extern "C" declaration is
// emscripten-specific and does not belong in obst.cpp .
extern "C" void OBST_EM_EXPORT(_platform_ui_button_help) () {
    platform_ui_button_help();
}
extern "C" void OBST_EM_EXPORT(_platform_ui_button_op) () {
    ui_button_op();
}
extern "C" void OBST_EM_EXPORT(_platform_ui_button_create) () {
    ui_button_create();
}
extern "C" void OBST_EM_EXPORT(_platform_ui_button_removeall) () {
    ui_button_removeall();
}
extern "C" void OBST_EM_EXPORT(_platform_ui_button_move) (float diff) {
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
char const* focusable_ids[] = {"create_nums", "create_form", "create_base", "create_bits", "create_vars", "op_node0", "op_node1", 0};
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

    if (focused and global_ui.novice_create_helper == 1) {
        // Hacky: Reset the create helper if an entry is focused
        global_ui.novice_create_helper = 2;
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
int OBST_EM_EXPORT(main) () {
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
    application_init();

    _platform_ui_button_typer();
    
    return 0;
}
