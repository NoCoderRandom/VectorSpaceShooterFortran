#include <SDL.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define VS_AUDIO_VOICES 24

typedef struct {
    float freq;
    float phase;
    float remaining;
    float volume;
} VsVoice;

static SDL_Window *g_window = NULL;
static SDL_Renderer *g_renderer = NULL;
static SDL_AudioDeviceID g_audio = 0;
static VsVoice g_voices[VS_AUDIO_VOICES];
static int g_sample_rate = 48000;
static int g_audio_ok = 0;
static int g_vsync_on = 0;

static void vs_audio_callback(void *userdata, Uint8 *stream, int len) {
    (void)userdata;
    int16_t *out = (int16_t *)stream;
    int frames = len / (int)(sizeof(int16_t) * 2);
    const float two_pi = 6.2831853071795864769f;

    memset(stream, 0, (size_t)len);

    for (int i = 0; i < frames; ++i) {
        float mixed = 0.0f;
        for (int v = 0; v < VS_AUDIO_VOICES; ++v) {
            VsVoice *voice = &g_voices[v];
            if (voice->remaining <= 0.0f) {
                continue;
            }

            float env = voice->remaining < 0.035f ? voice->remaining / 0.035f : 1.0f;
            mixed += sinf(voice->phase) * voice->volume * env;
            voice->phase += two_pi * voice->freq / (float)g_sample_rate;
            if (voice->phase > two_pi) {
                voice->phase -= two_pi;
            }
            voice->remaining -= 1.0f / (float)g_sample_rate;
        }

        if (mixed > 1.0f) mixed = 1.0f;
        if (mixed < -1.0f) mixed = -1.0f;

        int16_t sample = (int16_t)(mixed * 28000.0f);
        out[i * 2 + 0] = sample;
        out[i * 2 + 1] = sample;
    }
}

int vs_init(const char *title, int width, int height) {
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_TIMER) != 0) {
        fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
        return 0;
    }

    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1");

    g_window = SDL_CreateWindow(
        title,
        SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED,
        width,
        height,
        SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE);
    if (!g_window) {
        fprintf(stderr, "SDL_CreateWindow failed: %s\n", SDL_GetError());
        SDL_Quit();
        return 0;
    }

    g_renderer = SDL_CreateRenderer(
        g_window,
        -1,
        SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_TARGETTEXTURE);
    if (g_renderer) {
        g_vsync_on = 1;
    } else {
        g_renderer = SDL_CreateRenderer(g_window, -1, SDL_RENDERER_SOFTWARE);
        g_vsync_on = 0;
    }
    if (!g_renderer) {
        fprintf(stderr, "SDL_CreateRenderer failed: %s\n", SDL_GetError());
        SDL_DestroyWindow(g_window);
        g_window = NULL;
        SDL_Quit();
        return 0;
    }

    SDL_SetRenderDrawBlendMode(g_renderer, SDL_BLENDMODE_BLEND);

    SDL_AudioSpec want;
    SDL_AudioSpec have;
    SDL_zero(want);
    want.freq = 48000;
    want.format = AUDIO_S16SYS;
    want.channels = 2;
    want.samples = 1024;
    want.callback = vs_audio_callback;

    g_audio = SDL_OpenAudioDevice(NULL, 0, &want, &have, 0);
    if (g_audio != 0) {
        g_sample_rate = have.freq;
        g_audio_ok = 1;
        SDL_PauseAudioDevice(g_audio, 0);
    } else {
        g_audio_ok = 0;
        fprintf(stderr, "SDL audio disabled: %s\n", SDL_GetError());
    }

    return 1;
}

void vs_shutdown(void) {
    if (g_audio != 0) {
        SDL_CloseAudioDevice(g_audio);
        g_audio = 0;
    }
    if (g_renderer) {
        SDL_DestroyRenderer(g_renderer);
        g_renderer = NULL;
    }
    if (g_window) {
        SDL_DestroyWindow(g_window);
        g_window = NULL;
    }
    SDL_Quit();
}

int vs_pump_events(void) {
    SDL_Event event;
    int quit = 0;

    while (SDL_PollEvent(&event)) {
        if (event.type == SDL_QUIT) {
            quit = 1;
        }
    }

    return quit;
}

int vs_key_down(int scancode) {
    int count = 0;
    const Uint8 *state = SDL_GetKeyboardState(&count);
    if (scancode < 0 || scancode >= count) {
        return 0;
    }
    return state[scancode] ? 1 : 0;
}

uint32_t vs_ticks(void) {
    return SDL_GetTicks();
}

void vs_delay(int ms) {
    if (ms > 0) {
        SDL_Delay((Uint32)ms);
    }
}

void vs_get_draw_size(int *width, int *height) {
    if (!g_renderer) {
        *width = 0;
        *height = 0;
        return;
    }
    SDL_GetRendererOutputSize(g_renderer, width, height);
}

void vs_begin_frame(void) {
    if (!g_renderer) return;
    SDL_SetRenderDrawBlendMode(g_renderer, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(g_renderer, 0, 0, 0, 255);
    SDL_RenderClear(g_renderer);
}

void vs_present(void) {
    if (g_renderer) {
        SDL_RenderPresent(g_renderer);
    }
}

void vs_draw_line(int x1, int y1, int x2, int y2, int r, int g, int b, int a) {
    if (!g_renderer) return;
    SDL_SetRenderDrawColor(g_renderer, (Uint8)r, (Uint8)g, (Uint8)b, (Uint8)a);
    SDL_RenderDrawLine(g_renderer, x1, y1, x2, y2);
}

void vs_audio_beep(float freq, float seconds, float volume) {
    if (!g_audio_ok || g_audio == 0 || freq <= 0.0f || seconds <= 0.0f || volume <= 0.0f) {
        return;
    }

    if (volume > 1.0f) volume = 1.0f;

    SDL_LockAudioDevice(g_audio);
    int slot = 0;
    for (int i = 0; i < VS_AUDIO_VOICES; ++i) {
        if (g_voices[i].remaining <= 0.0f) {
            slot = i;
            break;
        }
    }
    g_voices[slot].freq = freq;
    g_voices[slot].phase = 0.0f;
    g_voices[slot].remaining = seconds;
    g_voices[slot].volume = volume;
    SDL_UnlockAudioDevice(g_audio);
}

int vs_vsync_active(void) {
    return g_vsync_on;
}

int vs_save_screenshot(const char *path) {
    if (!g_renderer) {
        return 0;
    }

    int width = 0;
    int height = 0;
    SDL_GetRendererOutputSize(g_renderer, &width, &height);
    if (width <= 0 || height <= 0) {
        return 0;
    }

    SDL_Surface *surface = SDL_CreateRGBSurfaceWithFormat(0, width, height, 32, SDL_PIXELFORMAT_ARGB8888);
    if (!surface) {
        fprintf(stderr, "SDL_CreateRGBSurfaceWithFormat failed: %s\n", SDL_GetError());
        return 0;
    }

    if (SDL_RenderReadPixels(g_renderer, NULL, SDL_PIXELFORMAT_ARGB8888, surface->pixels, surface->pitch) != 0) {
        fprintf(stderr, "SDL_RenderReadPixels failed: %s\n", SDL_GetError());
        SDL_FreeSurface(surface);
        return 0;
    }

    int ok = SDL_SaveBMP(surface, path) == 0 ? 1 : 0;
    if (!ok) {
        fprintf(stderr, "SDL_SaveBMP failed: %s\n", SDL_GetError());
    }
    SDL_FreeSurface(surface);
    return ok;
}
