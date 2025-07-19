/* Copyright 2023 Brian Low
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include QMK_KEYBOARD_H

enum sofle_layers {
    /* _M_XYZ = Mac Os, _W_XYZ = Win/Linux */
    _QWERTY,
    _LOWER,
    _RAISE,
    _ADJUST,
};

enum custom_keycodes {
    KC_QWERTY = SAFE_RANGE,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[_QWERTY] = LAYOUT(
  KC_GRV,   KC_1,   KC_2,    KC_3,    KC_4,    KC_5,                                KC_6,    KC_7,    KC_8,    KC_9,    KC_0,  XXXXXXX,
  KC_TAB,   KC_Q,   KC_W,    KC_E,    KC_R,    KC_T,                                KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,  KC_LBRC,
  KC_ESC,   KC_A,   KC_S,    KC_D,    KC_F,    KC_G,                                KC_H,    KC_J,    KC_K,    KC_L, KC_SCLN,  KC_QUOT,
  KC_LSFT,  KC_Z,   KC_X,    KC_C,    KC_V,    KC_B,      KC_MUTE,  XXXXXXX,        KC_N,    KC_M, KC_COMM,  KC_DOT, KC_SLSH,  KC_BSPC,
                    KC_LALT, KC_LGUI, KC_LCTL, MO(_LOWER), KC_SPC,   KC_ENT,  MO(_RAISE), KC_LSFT, XXXXXXX, XXXXXXX
),
[_LOWER] = LAYOUT(
  _______,   KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,                       KC_F6,   KC_F7,   KC_F8,   KC_F9,  KC_F10,  KC_F11,
   KC_GRV, KC_EXLM,   KC_AT, KC_HASH,  KC_DLR, KC_PERC,                     KC_CIRC, KC_AMPR, KC_ASTR, XXXXXXX, XXXXXXX,  KC_F12,
  KC_TILD, XXXXXXX, KC_UNDS, KC_EQL,   KC_LT,   KC_GT,                     KC_LPRN, KC_RPRN, KC_QUOT, KC_DQUO, KC_PIPE,  XXXXXXX,
  _______, XXXXXXX, KC_MINS, KC_PLUS, KC_LCBR, KC_RCBR, _______,   _______, KC_LBRC, KC_RBRC, KC_SCLN, KC_COLN, KC_BSLS, _______,
                    _______, _______, _______, _______, _______,   _______, _______, _______, _______, _______
),
[_RAISE] = LAYOUT(
  _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                          _______, _______, _______, _______, _______, _______,
  _______,    KC_1,    KC_2,    KC_3,    KC_4,    KC_5,                             KC_6,    KC_7,    KC_8,    KC_9,    KC_0, _______,
  _______, XXXXXXX, XXXXXXX, XXXXXXX, KC_CAPS, KC_RALT,                          KC_LEFT, KC_DOWN,   KC_UP, KC_RGHT, XXXXXXX, _______,
  _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_PSCR,  _______,       _______, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, _______,
                    _______, _______, _______, _______, _______,        _______, _______, _______, _______, _______
),
[_ADJUST] = LAYOUT(
XXXXXXX, XXXXXXX,   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,                       XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
XXXXXXX, XXXXXXX,   XXXXXXX, XXXXXXX, CG_TOGG, XXXXXXX,                       UG_NEXT, UG_TOGG, UG_VALU, UG_VALD, XXXXXXX, XXXXXXX,
QK_BOOT, XXXXXXX,   XXXXXXX, DF(_QWERTY), XXXXXXX, XXXXXXX,                   XXXXXXX, KC_VOLD, KC_MUTE, KC_VOLU, XXXXXXX, XXXXXXX,
_______, XXXXXXX,   XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,     XXXXXXX, XXXXXXX, KC_MPRV, KC_MPLY, KC_MNXT, XXXXXXX, XXXXXXX,
                    _______, _______, _______, _______, _______,     _______, _______, _______, _______, _______
)
};

#if defined(ENCODER_MAP_ENABLE)
const uint16_t PROGMEM encoder_map[][NUM_ENCODERS][NUM_DIRECTIONS] = {
    [_QWERTY] = { ENCODER_CCW_CW(KC_VOLD, KC_VOLU), ENCODER_CCW_CW(KC_PGDN, KC_PGUP) },
    [_LOWER]  = { ENCODER_CCW_CW(UG_HUEU, UG_HUED), ENCODER_CCW_CW(UG_SATU, UG_SATD) },
    [_RAISE]  = { ENCODER_CCW_CW(KC_VOLD, KC_VOLU), ENCODER_CCW_CW(KC_PGDN, KC_PGUP) },
    [_ADJUST] = { ENCODER_CCW_CW(KC_VOLD, KC_VOLU), ENCODER_CCW_CW(KC_MPRV, KC_MNXT) },
};
#endif

#ifdef OLED_ENABLE

static void print_status_narrow(void) {
    // Print current mode
    oled_write_P(PSTR("M:"), false);
    if (keymap_config.swap_lctl_lgui) {
        oled_write_ln_P(PSTR("Mac"), false);
    } else {
        oled_write_ln_P(PSTR("Win"), false);
    }

    // Print current layer
    oled_write_P(PSTR("L:"), false);
    switch (get_highest_layer(layer_state)) {
        case _QWERTY:
            oled_write_ln_P(PSTR("Bas"), false);
            break;
        case _RAISE:
            oled_write_ln_P(PSTR("Rai"), false);
            break;
        case _LOWER:
            oled_write_ln_P(PSTR("Low"), false);
            break;
        case _ADJUST:
            oled_write_ln_P(PSTR("Adj"), false);
            break;
        default:
            oled_write_ln_P(PSTR("Unk"), false);
    }

    led_t led_usb_state = host_keyboard_led_state();
    oled_write_ln_P(PSTR("CPSLK"), led_usb_state.caps_lock);
}

static void render_logo(void) {
    static const char PROGMEM qmk_logo[] = {
        0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8a,0x8b,0x8c,0x8d,0x8e,0x8f,0x90,0x91,0x92,0x93,0x94,
        0xa0,0xa1,0xa2,0xa3,0xa4,0xa5,0xa6,0xa7,0xa8,0xa9,0xaa,0xab,0xac,0xad,0xae,0xaf,0xb0,0xb1,0xb2,0xb3,0xb4,
        0xc0,0xc1,0xc2,0xc3,0xc4,0xc5,0xc6,0xc7,0xc8,0xc9,0xca,0xcb,0xcc,0xcd,0xce,0xcf,0xd0,0xd1,0xd2,0xd3,0xd4,0
    };

    oled_write_P(qmk_logo, false);
}

oled_rotation_t oled_init_user(oled_rotation_t rotation) {
    if (is_keyboard_master()) {
        return OLED_ROTATION_270;
    } else {
        return OLED_ROTATION_180;
    }
    return rotation;
}

bool oled_task_user(void) {
    if (is_keyboard_master()) {
        print_status_narrow();
    } else {
        render_logo();
    }
    return false;
}

#endif

layer_state_t layer_state_set_user(layer_state_t state) {
    return update_tri_layer_state(state, _LOWER, _RAISE, _ADJUST);
}
