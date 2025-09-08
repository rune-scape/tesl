//taken from https://github.com/agauniyal/rang

#pragma once

#include <cstdio>

namespace tcolor {
    // For better compability with most of terminals do not use any style settings
    // except of reset, bold and reversed.
    // Note that on Windows terminals bold style is same as fgB color.
    enum class style {
        reset     = 0,
        bold      = 1,
        dim       = 2,
        italic    = 3,
        underline = 4,
        blink     = 5,
        rblink    = 6,
        reversed  = 7,
        conceal   = 8,
        crossed   = 9
    };

    enum class fg {
        black   = 30,
        red     = 31,
        green   = 32,
        yellow  = 33,
        blue    = 34,
        magenta = 35,
        cyan    = 36,
        white   = 37,

        gray           = 90,
        bright_red     = 91,
        bright_green   = 92,
        bright_yellow  = 93,
        bright_blue    = 94,
        bright_magenta = 95,
        bright_cyan    = 96,
        bright_white   = 97,

        reset   = 39
    };

    enum class bg {
        black   = 40,
        red     = 41,
        green   = 42,
        yellow  = 43,
        blue    = 44,
        magenta = 45,
        cyan    = 46,
        white   = 47,

        gray           = 100,
        bright_red     = 101,
        bright_green   = 102,
        bright_yellow  = 103,
        bright_blue    = 104,
        bright_magenta = 105,
        bright_cyan    = 106,
        bright_white   = 107,

        reset   = 49
    };

    // Behaviour of tcolor function calls
    // Use tcolor::setControlMode to set tcolor control mode
    enum class control {
        Off   = 0,  // toggle off tcolor style/color calls
        Auto  = 1,  // (Default) autodect terminal and colorize if needed
        Force = 2  // force ansi color output to non terminal streams
    };

    // Windows Terminal Mode
    // Use tcolor::setWinTermMode to explicitly set terminal API for Windows
    // Calling tcolor::setWinTermMode have no effect on other OS
    enum class winTerm {
        Auto   = 0,  // (Default) automatically detects wheter Ansi or Native API
        Ansi   = 1,  // Force use Ansi API
        Native = 2  // Force use Native API
    };

    void setWinColorNative(FILE *f, fg value);
    void setWinColorNative(FILE *f, bg value);

    void setWinStyleNative(FILE *f, style value);

    void setColor(FILE *f, fg value);
    void setColor(FILE *f, bg value);

    void setStyle(FILE *f, style value);

    void setWinTermMode(const winTerm value);
    void setControlMode(const control value);

}  // namespace tcolor
