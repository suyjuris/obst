#pragma once

struct Key {
    enum Key_type: u8 {
        NONE, TEXT, SPECIAL, MOUSE, GENERAL
    };
    enum Key_special: u8 {
        INVALID, ESCAPE, ARROW_L, ARROW_R, ARROW_D, ARROW_U,
        HOME, END, PAGE_U, PAGE_D, TAB, SHIFT_TAB, DELETE, BACKSPACE, RETURN,
        C_COPY, C_PASTE, C_CUT, C_SELECTALL, C_UNDO, C_REDO, C_QUIT, C_SAVE,
        F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
        SPECIAL_COUNT
    };
    enum Key_flags: u8 {
        MOD_SHIFT = 1, MOD_CTRL = 2,
    };
    static constexpr char const* key_special_names[] = {
        "invalid", "escape", "arrow_l", "arrow_r", "arrow_d", "arrow_u",
        "home", "end", "page_u", "page_d", "tab", "shift_tab", "delete", "backspace", "return",
        "c_copy", "c_paste", "c_cut", "c_selectall", "c_undo", "c_redo", "c_quit", "c_save",
        "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12"
    };
    enum Mouse_action: u8 {
        LEFT_DOWN, LEFT_UP, MOTION, SCROLL_UP, SCROLL_DOWN
    };

    enum General_type: u8 {
        FOCUS_IN, FOCUS_OUT, GENERAL_COUNT
    };

    u8 type = Key::NONE;
    union {
        u8 text[15];
        struct { u8 special; u8 flags; s64 data; };
        s32 mouse[3];
        u8 general;
    };

    static Key create_text(Array_t<char> text_) {
        assert(text_.size+1 < (s64)sizeof(Key::text));
        Key result;
        result.type = Key::TEXT;
        memcpy(result.text, text_.data, text_.size);
        result.text[text_.size] = 0;
        return result;
    }
    static Key create_special(u8 special, u8 flags, s32 data = -1) {
        assert(special != INVALID and special < SPECIAL_COUNT);
        Key result;
        result.type = Key::SPECIAL;
        result.special = special;
        result.flags = flags;
        result.data = data;
        return result;
    }
    static Key create_mouse(u8 action, s64 x, s64 y) {
        assert((s32)x == x and (s32)y == y);
        Key result;
        result.type = Key::MOUSE;
        result.mouse[0] = (s32)action;
        result.mouse[1] = (s32)x;
        result.mouse[2] = (s32)y;
        return result;
    }
    static Key create_general(u8 general) {
        assert(general < GENERAL_COUNT);
        Key result;
        result.type = Key::GENERAL;
        result.general = general;
        return result;
    }

    void get_mouse_param(u8* action, s64* x, s64* y) {
        assert(type == Key::MOUSE);
        if (action) *action = (u8)mouse[0];
        if (x) *x = mouse[1];
        if (y) *y = mouse[2];
    }
};

constexpr char const* Key::key_special_names[];

void print_key(Key key) {
    if (key.type == Key::NONE) {
        printf("{NONE}");
    } else if (key.type == Key::TEXT) {
        printf("{\"%s\" %02x}", key.text, (u8)key.text[0]);
    } else if (key.type == Key::SPECIAL) {
        assert(key.special < Key::SPECIAL_COUNT);
        printf("{%s}", Key::key_special_names[key.special]);
    } else {
        assert(false);
    }
}

namespace Text_fmt {

//@Cleanup: Replace NOSPACE with SPACE
enum Flags: u64 {
    PARAGRAPH = 1, // Indicates a paragraph break at the end of the item
    PARAGRAPH_CLOSE = 2,
    NEWLINE = 4,
    NOSPACE = 8, // Do not leave a space after the word. Internal flag for lui_text_draw
    STICKY = 16, // Do not break up the word at this point
    HEADER = 32, // Corresponds to <h4>, draw text as title
    BOLD = 64,
    ITALICS = 128,
    SMALL = 256,
    SANS = 512,
    COMPACT = 1024,
    RED = 2048,
    INDENTED = 4096,
    ITEMIZED = 8192,
    GROUP_SPACING = PARAGRAPH | PARAGRAPH_CLOSE | NEWLINE | NOSPACE | STICKY,
    GROUP_BREAKING = PARAGRAPH | PARAGRAPH_CLOSE | NEWLINE,
    GROUP_DRAWING = INDENTED | RED | ITEMIZED,
};
enum Slots: s64 {
    SLOT_CONTEXT, SLOT_CONTEXT_FRAME, SLOT_BDDINFO, SLOT_HELPTEXT, SLOT_ERRORINFO,
    SLOT_PLATFORM_FIRST
};

};

struct Text_box {
    float x0 = 0.f, y0 = 0.f, x1 = 0.f, y1 = 0.f;
    float s0 = 0.f, t0 = 0.f, s1 = 0.f, t1 = 0.f;
    float advance = 0.f;
    u8 font;
    u32 flags = 0; // Same as the spacing flags in Text_fmt::Flags
    
    static_assert(Text_fmt::GROUP_SPACING >> 32 == 0, "32-bit not sufficient or Text_box flags");
};

// Forward declarations for the application layer
void platform_ui_error_report(Array_t<u8> msg);
void platform_ui_error_clear();
void platform_text_prepare(int size, float small_frac, Array_t<Text_box>* offsets, float* ascent);
Array_t<u8> platform_ui_value_get(u8 elem);
void platform_ui_value_free(Array_t<u8> data);
void platform_ui_cursor_set(u8 elem, s64 cursor, s64 cursor_row, s64 cursor_col);
void platform_ui_bddinfo_hide();
void platform_ui_bddinfo_show(float x, float y, float pad);
double platform_now();
void platform_mouse_position(float* out_x, float* out_y);
void platform_ui_button_help ();
bool platform_ui_help_active ();
void platform_operations_enable(u32 bdd);
void platform_operations_disable();
void platform_main_loop_active(bool is_active);
void platform_set_cursor(u8 type);
Array_t<u8> platform_clipboard_get(s64 index);
void platform_clipboard_free(s64 index);
void platform_clipboard_set(u8 type, Array_t<u8> data);

void platform_fmt_init();
void platform_fmt_begin(u64 flags);
void platform_fmt_end(u64 flags);
void platform_fmt_text(u64 flags, Array_t<u8> text);
void platform_fmt_text(u64 flags, char const* s);
void platform_fmt_store(s64 slot);
void platform_fmt_store_simple(u64 flags, char const* str, s64 slot);
void platform_fmt_store_simple(u64 flags, Array_t<u8> str, s64 slot);
void platform_fmt_draw(s64 slot, s64 x, s64 y, s64 w, s64* x_out, s64* y_out, bool only_measure=false, s64* xw_out=nullptr);
void platform_fmt_store_copy(s64 slot_into, s64 slot_from);
