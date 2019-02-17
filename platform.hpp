#pragma once

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


// Forward declarations for the application layer
void platform_ui_error_report(Array_t<u8> msg);
void platform_ui_error_clear();
int platform_text_prepare(int size, int w, float* offsets);
Array_t<u8> platform_ui_get_value(u8 elem);
void platform_ui_bddinfo_hide();
void platform_ui_bddinfo_show(float x, float y, Array_t<u8> text);
void platform_ui_context_set(Array_t<u8> text, int frame, int frame_max);
double platform_now();
void platform_mouse_position(float* out_x, float* out_y);
void platform_ui_button_help ();
void platform_operations_enable(u32 bdd);
void platform_operations_disable();
void platform_main_loop_active(bool is_active);
