// taken from https://github.com/agauniyal/rang

#include "tcolor.hpp"
#include <fmt/format.h>

#if defined(__unix__) || defined(__unix) || defined(__linux__)
#define TCOLOR_OS_LINUX
#elif defined(WIN32) || defined(_WIN32) || defined(_WIN64)
#define TCOLOR_OS_WIN
#elif defined(__APPLE__) || defined(__MACH__)
#define TCOLOR_OS_MAC
#endif

#if defined(TCOLOR_OS_LINUX) || defined(TCOLOR_OS_MAC)
#include <unistd.h>

#elif defined(TCOLOR_OS_WIN)

#if defined(_WIN32_WINNT) && (_WIN32_WINNT < 0x0600)
#error "Please include tcolor.hpp before any windows system headers or set _WIN32_WINNT at least to _WIN32_WINNT_VISTA"
#elif !defined(_WIN32_WINNT)
#define _WIN32_WINNT _WIN32_WINNT_VISTA
#endif

#include <windows.h>
#include <io.h>
#include <memory>
#include <string>
#include <mutex>

// Only defined in windows 10 onwards, redefining in lower windows since it
// doesn't gets used in lower versions
// https://docs.microsoft.com/en-us/windows/console/getconsolemode
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif

#endif

#include <atomic>
#include <cstdlib>
#include <cstring>

namespace tcolor {

namespace detail {
    static std::atomic<control> controlMode = control::Auto;
    static std::atomic<winTerm> winTermMode = winTerm::Auto;

    bool supportsColorImpl() {
#if defined(TCOLOR_OS_LINUX) || defined(TCOLOR_OS_MAC)
        const char *Terms[] = {"ansi", "color", "console", "cygwin", "gnome", "konsole", "kterm", "linux", "msys", "putty", "rxvt", "screen", "vt100", "xterm"};
        const char *env_p = std::getenv("TERM");
        if (env_p == nullptr) {
            return false;
        }
        for (const char *term : Terms) {
            if (std::strstr(env_p, term) != nullptr) {
                return true;
            }
        }
        return false;
#elif defined(TCOLOR_OS_WIN)
        // All windows versions support colors through native console methods
        return true;
#else
        return false;
#endif
    }

    bool supportsColor() {
        static bool result = supportsColorImpl();
        return result;
    }

    void setColorAnsi(FILE *f, int value) {
        fmt::print(f, "\033[{}m", value);
    }

#ifdef TCOLOR_OS_WIN
    bool isMsysPty(int fd) {
        // Dynamic load for binary compability with old Windows
        const auto ptrGetFileInformationByHandleEx = reinterpret_cast<decltype(&GetFileInformationByHandleEx)>(GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")), "GetFileInformationByHandleEx"));
        if (!ptrGetFileInformationByHandleEx) {
            return false;
        }

        HANDLE h = reinterpret_cast<HANDLE>(_get_osfhandle(fd));
        if (h == INVALID_HANDLE_VALUE) {
            return false;
        }

        // Check that it's a pipe:
        if (GetFileType(h) != FILE_TYPE_PIPE) {
            return false;
        }

        // POD type is binary compatible with FILE_NAME_INFO from WinBase.h
        // It have the same alignment and used to avoid UB in caller code
        struct MY_FILE_NAME_INFO {
            DWORD FileNameLength;
            WCHAR FileName[MAX_PATH];
        };

        MY_FILE_NAME_INFO pNameInfo;

        // Check pipe name is template of
        // {"cygwin-","msys-"}XXXXXXXXXXXXXXX-ptyX-XX
        if (!ptrGetFileInformationByHandleEx(h, FileNameInfo, &pNameInfo, sizeof(MY_FILE_NAME_INFO))) {
            return false;
        }
        std::wstring name(pNameInfo.FileName, pNameInfo.FileNameLength / sizeof(WCHAR));
        if ((name.find(L"msys-") == std::wstring::npos && name.find(L"cygwin-") == std::wstring::npos) || name.find(L"-pty") == std::wstring::npos) {
            return false;
        }

        return true;
    }

    bool isTerminal(FILE *f) {
        if (f == stdout) {
            static const bool cout_term = (_isatty(_fileno(stdout)) || isMsysPty(_fileno(stdout)));
            return cout_term;
        } else if (f == stderr) {
            static const bool cerr_term = (_isatty(_fileno(stderr)) || isMsysPty(_fileno(stderr)));
            return cerr_term;
        }
        return false;
    }

    struct SGR {  // Select Graphic Rendition parameters for Windows console
        BYTE fgColor;  // foreground color (0-15) lower 3 rgb bits + intense bit
        BYTE bgColor;  // background color (0-15) lower 3 rgb bits + intense bit
        BYTE bold;  // emulated as FOREGROUND_INTENSITY bit
        BYTE underline;  // emulated as BACKGROUND_INTENSITY bit
        BOOLEAN inverse;  // swap foreground/bold & background/underline
        BOOLEAN conceal;  // set foreground/bold to background/underline
    };

    enum class AttrColor : BYTE {  // Color attributes for console screen buffer
        black   = 0,
        red     = 4,
        green   = 2,
        yellow  = 6,
        blue    = 1,
        magenta = 5,
        cyan    = 3,
        gray    = 7
    };

    HANDLE getConsoleHandle(FILE *f) {
        if (f == stdout) {
            static const HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
            return hStdout;
        } else if (f == stderr) {
            static const HANDLE hStderr = GetStdHandle(STD_ERROR_HANDLE);
            return hStderr;
        }
        return INVALID_HANDLE_VALUE;
    }

    bool setWinTermAnsiColors(FILE *f) {
        HANDLE h = getConsoleHandle(f);
        if (h == INVALID_HANDLE_VALUE) {
            return false;
        }
        DWORD dwMode = 0;
        if (!GetConsoleMode(h, &dwMode)) {
            return false;
        }
        dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
        if (!SetConsoleMode(h, dwMode)) {
            return false;
        }
        return true;
    }

    bool supportsAnsi(FILE *f) {
        if (f == stdout) {
            static const bool cout_ansi = (isMsysPty(_fileno(stdout)) || setWinTermAnsiColors(f));
            return cout_ansi;
        } else if (f == stderr) {
            static const bool cerr_ansi = (isMsysPty(_fileno(stderr)) || setWinTermAnsiColors(f));
            return cerr_ansi;
        }
        return false;
    }

    const SGR defaultStateImpl() {
        CONSOLE_SCREEN_BUFFER_INFO info;
        WORD attrib = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
        if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),
                                        &info)
            || GetConsoleScreenBufferInfo(GetStdHandle(STD_ERROR_HANDLE),
                                            &info)) {
            attrib = info.wAttributes;
        }
        SGR sgr     = { 0, 0, 0, 0, FALSE, FALSE };
        sgr.fgColor = attrib & 0x0F;
        sgr.bgColor = (attrib & 0xF0) >> 4;
        return sgr;
    }

    static const SGR defaultState = defaultStateImpl();
    static SGR currentState = defaultState;
    static std::mutex currentStateMutex;

    BYTE ansi2attr(BYTE rgb) {
        static const AttrColor rev[8] = {AttrColor::black, AttrColor::red, AttrColor::green, AttrColor::yellow, AttrColor::blue, AttrColor::magenta, AttrColor::cyan, AttrColor::gray};
        return static_cast<BYTE>(rev[rgb]);
    }

    void setWinSGR(tcolor::bg col, SGR &state) {
        if (col != tcolor::bg::reset) {
            if (static_cast<int>(col) >= 100) {
                state.bgColor = (BACKGROUND_INTENSITY >> 4) | ansi2attr(static_cast<BYTE>(static_cast<int>(col) - 100));
            } else {
                state.bgColor = ansi2attr(static_cast<BYTE>(static_cast<int>(col) - 40));
            }
        } else {
            state.bgColor = defaultState.bgColor;
        }
    }

    void setWinSGR(tcolor::fg col, SGR &state) {
        if (col != tcolor::fg::reset) {
            if (static_cast<int>(col) >= 90) {
                state.fgColor = FOREGROUND_INTENSITY | ansi2attr(static_cast<BYTE>(static_cast<int>(col) - 90));
            } else {
                state.fgColor = ansi2attr(static_cast<BYTE>(static_cast<int>(col) - 30));
            }
        } else {
            state.fgColor = defaultState.fgColor;
        }
    }

    void setWinSGR(tcolor::style style, SGR &state) {
        switch (style) {
            case tcolor::style::reset:
                state = defaultState;
                break;
            case tcolor::style::bold:
                state.bold = FOREGROUND_INTENSITY;
                break;
            case tcolor::style::underline:
            case tcolor::style::blink:
                state.underline = BACKGROUND_INTENSITY;
                break;
            case tcolor::style::reversed:
                state.inverse = TRUE;
                break;
            case tcolor::style::conceal:
                state.conceal = TRUE;
                break;
            default:
                break;
        }
    }

    WORD SGR2Attr(const SGR &state) {
        WORD attrib = 0;
        if (state.conceal) {
            if (state.inverse) {
                attrib = (state.fgColor << 4) | state.fgColor;
                if (state.bold) attrib |= FOREGROUND_INTENSITY | BACKGROUND_INTENSITY;
            } else {
                attrib = (state.bgColor << 4) | state.bgColor;
                if (state.underline) attrib |= FOREGROUND_INTENSITY | BACKGROUND_INTENSITY;
            }
        } else if (state.inverse) {
            attrib = (state.fgColor << 4) | state.bgColor;
            if (state.bold) attrib |= BACKGROUND_INTENSITY;
            if (state.underline) attrib |= FOREGROUND_INTENSITY;
        } else {
            attrib = state.fgColor | (state.bgColor << 4) | state.bold | state.underline;
        }
        return attrib;
    }

    template<typename T>
    void setWinColorNative(FILE *f, T value) {
        const HANDLE h = getConsoleHandle(f);
        if (h != INVALID_HANDLE_VALUE) {
            std::lock_guard lock(currentStateMutex);
            setWinSGR(value, currentState);
            // Out all buffered text to console with previous settings:
            fflush(f);
            SetConsoleTextAttribute(h, SGR2Attr(currentState));
        }
    }

    template<typename T>
    void setColor(FILE *f, T value) {
        if (winTermMode == winTerm::Auto) {
            setWinColorNative(f, value);
        } else if (winTermMode == winTerm::Ansi) {
            setColorAnsi(f, static_cast<int>(value));
        } else {
            setWinColorNative(f, value);
        }
    }

#else
    bool isTerminal(FILE *f) {
#if defined(TCOLOR_OS_LINUX) || defined(TCOLOR_OS_MAC)
        if (f == stdout) {
            static const bool cout_term = isatty(fileno(stdout)) != 0;
            return cout_term;
        } else if (f == stderr) {
            static const bool cerr_term = isatty(fileno(stderr)) != 0;
            return cerr_term;
        }
#endif
        return false;
    }

    template<typename T>
    void setWinColorNative(FILE *f, T value) {
        // do nothing
    }

    template<typename T>
    void setColor(FILE *f, T value) {
        setColorAnsi(f, static_cast<int>(value));
    }
#endif

}  // namespace detail

void setWinColorNative(FILE *f, fg value) {
    detail::setWinColorNative(f, value);
}

void setWinColorNative(FILE *f, bg value) {
    detail::setWinColorNative(f, value);
}

void setWinStyleNative(FILE *f, style value) {
    detail::setWinColorNative(f, value);
}

void setColor(FILE *f, fg value) {
    detail::setColor(f, value);
}

void setColor(FILE *f, bg value) {
    detail::setColor(f, value);
}

void setStyle(FILE *f, style value) {
    detail::setColor(f, value);
}

void setWinTermMode(const winTerm value) {
    detail::winTermMode = value;
}

void setControlMode(const control value) {
    detail::controlMode = value;
}

}
