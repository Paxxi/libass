/*
 * Copyright (C) 2015 Stephan Vedder <stephan.vedder@gmail.com>
 *
 * This file is part of libass.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
//#define COBJMACROS

#if HAVE_CONFIG_H
#include "config.h"
#endif
#include "ass_compat.h"

#include <initguid.h>
#include <ole2.h>
#include <shobjidl.h>
#include <dwrite.h>

//#include "dwrite_c.h"

#include "ass_directwrite.h"
#include "ass_utils.h"
#include "ass_fontselect.h"

#define NAME_MAX_LENGTH 256
#define FALLBACK_DEFAULT_FONT L"Arial"

static const ASS_FontMapping font_substitutions[] = {
    {"sans-serif", "Arial"},
    {"serif", "Times New Roman"},
    {"monospace", "Courier New"}
};

/*
 * The private data stored for every font, detected by this backend.
 */
typedef struct {
    struct IDWriteFont *font;
    struct IDWriteFontFace *face;
    struct IDWriteFontFileStream *stream;
} FontPrivate;

typedef struct {
    HMODULE directwrite_lib;
    struct IDWriteFactory *factory;
} ProviderPrivate;

/**
 * Custom text renderer class for logging the fonts used. It does not
 * actually render anything or do anything apart from that.
 */

interface DWRITE_DECLARE_INTERFACE("92BA64F5-A48F-4816-B373-ACDB002CCD01") FallbackLogTextRenderer : public IDWriteTextRenderer 
{
public:
  HRESULT __stdcall QueryInterface(const IID& riid, void** ppvObject) override;
  ULONG __stdcall AddRef() override;
  ULONG __stdcall Release() override;
  HRESULT __stdcall IsPixelSnappingDisabled(void* clientDrawingContext, BOOL* isDisabled) override;
  HRESULT __stdcall GetCurrentTransform(void* clientDrawingContext, DWRITE_MATRIX* transform) override;
  HRESULT __stdcall GetPixelsPerDip(void* clientDrawingContext, FLOAT* pixelsPerDip) override;
  HRESULT __stdcall DrawGlyphRun(void* clientDrawingContext, FLOAT baselineOriginX, FLOAT baselineOriginY, DWRITE_MEASURING_MODE measuringMode, DWRITE_GLYPH_RUN const* glyphRun, DWRITE_GLYPH_RUN_DESCRIPTION const* glyphRunDescription, IUnknown* clientDrawingEffect) override;
  HRESULT __stdcall DrawUnderline(void* clientDrawingContext, FLOAT baselineOriginX, FLOAT baselineOriginY, DWRITE_UNDERLINE const* underline, IUnknown* clientDrawingEffect) override;
  HRESULT __stdcall DrawStrikethrough(void* clientDrawingContext, FLOAT baselineOriginX, FLOAT baselineOriginY, DWRITE_STRIKETHROUGH const* strikethrough, IUnknown* clientDrawingEffect) override;
  HRESULT __stdcall DrawInlineObject(void* clientDrawingContext, FLOAT originX, FLOAT originY, IDWriteInlineObject* inlineObject, BOOL isSideways, BOOL isRightToLeft, IUnknown* clientDrawingEffect) override;
  IDWriteFactory* dw_factory;
private:
  static DWORD ref_count;
};

DWORD FallbackLogTextRenderer::ref_count = 0;

// IUnknown methods
HRESULT FallbackLogTextRenderer::QueryInterface(const IID& riid, void** ppvObject)
{
    if (IsEqualGUID(riid, __uuidof(IDWriteTextRenderer))
        || IsEqualGUID(riid, __uuidof(IDWritePixelSnapping))
        || IsEqualGUID(riid, __uuidof(IUnknown))) {
        *ppvObject = this;
    } else {
        *ppvObject = NULL;
        return E_FAIL;
    }

    AddRef();
    return S_OK;
}

ULONG FallbackLogTextRenderer::AddRef()
{
    return InterlockedIncrement(&ref_count);
}

ULONG FallbackLogTextRenderer::Release()
{
    unsigned long new_count = InterlockedDecrement(&ref_count);
    if (new_count == 0) {
      delete this;
        return 0;
    }

    return new_count;
}

long FallbackLogTextRenderer::IsPixelSnappingDisabled(void* clientDrawingContext, BOOL* isDisabled)
{
    *isDisabled = true;
    return S_OK;
}

HRESULT FallbackLogTextRenderer::GetCurrentTransform(void* clientDrawingContext, DWRITE_MATRIX* transform)
{
  return E_NOTIMPL;
}

HRESULT FallbackLogTextRenderer::GetPixelsPerDip(void* clientDrawingContext, FLOAT* pixelsPerDip)
{
  return E_NOTIMPL;
}

HRESULT FallbackLogTextRenderer::DrawGlyphRun(void* clientDrawingContext, FLOAT baselineOriginX, FLOAT baselineOriginY, DWRITE_MEASURING_MODE measuringMode, DWRITE_GLYPH_RUN const* glyphRun, DWRITE_GLYPH_RUN_DESCRIPTION const* glyphRunDescription, IUnknown* clientDrawingEffect)
{
    HRESULT hr;
    struct IDWriteFontCollection *font_coll = NULL;
    struct IDWriteFont **font = (struct IDWriteFont **)clientDrawingContext;

    hr = dw_factory->GetSystemFontCollection(&font_coll, FALSE);
    if (FAILED(hr))
        return E_FAIL;

    hr = font_coll->GetFontFromFontFace(glyphRun->fontFace, font);
    if (FAILED(hr))
        return E_FAIL;

    return S_OK;
}

HRESULT FallbackLogTextRenderer::DrawUnderline(void* clientDrawingContext, FLOAT baselineOriginX, FLOAT baselineOriginY, DWRITE_UNDERLINE const* underline, IUnknown* clientDrawingEffect)
{
  return S_OK;
}

HRESULT FallbackLogTextRenderer::DrawStrikethrough(void* clientDrawingContext, FLOAT baselineOriginX, FLOAT baselineOriginY, DWRITE_STRIKETHROUGH const* strikethrough, IUnknown* clientDrawingEffect)
{
  return S_OK;
}

HRESULT FallbackLogTextRenderer::DrawInlineObject(void* clientDrawingContext, FLOAT originX, FLOAT originY, IDWriteInlineObject* inlineObject, BOOL isSideways, BOOL isRightToLeft, IUnknown* clientDrawingEffect)
{
  return S_OK;
}

static void init_FallbackLogTextRenderer(FallbackLogTextRenderer *r,
                                         struct IDWriteFactory *factory)
{
  r = new FallbackLogTextRenderer();
  r->dw_factory = factory;
  r->AddRef();
}

/*
 * This function is called whenever a font is accessed for the
 * first time. It will create a FontFace for metadata access and
 * memory reading, which will be stored within the private data.
 */
static bool init_font_private_face(FontPrivate *priv)
{
    HRESULT hr;
    struct IDWriteFontFace *face;

    if (priv->face != NULL)
        return true;

    hr = priv->font->CreateFontFace(&face);
    if (FAILED(hr) || !face)
        return false;

    priv->face = face;
    return true;
}

/*
 * This function is called whenever a font is used for the first
 * time. It will create a FontStream for memory reading, which
 * will be stored within the private data.
 */
static bool init_font_private_stream(FontPrivate *priv)
{
    HRESULT hr = S_OK;
    struct IDWriteFontFile *file = NULL;
    struct IDWriteFontFileStream *stream = NULL;
    struct IDWriteFontFileLoader *loader = NULL;
    UINT32 n_files = 1;
    const void *refKey = NULL;
    UINT32 keySize = 0;

    if (priv->stream != NULL)
        return true;

    if (!init_font_private_face(priv))
        return false;

    /* DirectWrite only supports one file per face */
    hr = priv->face->GetFiles(&n_files, &file);
    if (FAILED(hr) || !file)
        return false;

    hr = file->GetReferenceKey(&refKey, &keySize);
    if (FAILED(hr)) {
        file->Release();
        return false;
    }

    hr = file->GetLoader(&loader);
    if (FAILED(hr) || !loader) {
      file->Release();
        return false;
    }

    hr = loader->CreateStreamFromKey(refKey, keySize, &stream);
    if (FAILED(hr) || !stream) {
      file->Release();
        return false;
    }

    priv->stream = stream;
    file->Release();

    return true;
}

/*
 * Read a specified part of a fontfile into memory.
 * If the font wasn't used before first creates a
 * FontStream and save it into the private data for later usage.
 * If the parameter "buf" is NULL libass wants to know the
 * size of the Fontfile
 */
static size_t get_data(void *data, unsigned char *buf, size_t offset,
                       size_t length)
{
    HRESULT hr = S_OK;
    FontPrivate *priv = (FontPrivate *) data;
    const void *fileBuf = NULL;
    void *fragContext = NULL;

    if (!init_font_private_stream(priv))
        return 0;

    if (buf == NULL) {
        UINT64 fileSize;
        hr = priv->stream->GetFileSize(&fileSize);
        if (FAILED(hr))
            return 0;

        return fileSize;
    }

    hr = priv->stream->ReadFileFragment(&fileBuf, offset,
      length, &fragContext);

    if (FAILED(hr) || !fileBuf)
        return 0;

    memcpy(buf, fileBuf, length);

    priv->stream->ReleaseFileFragment(fragContext);

    return length;
}

/*
 * Check whether the font contains PostScript outlines.
 */
static bool check_postscript(void *data)
{
    FontPrivate *priv = (FontPrivate *) data;

    if (!init_font_private_face(priv))
        return false;

    enum DWRITE_FONT_FACE_TYPE type = priv->face->GetType();
    return type == DWRITE_FONT_FACE_TYPE_CFF ||
           type == DWRITE_FONT_FACE_TYPE_RAW_CFF ||
           type == DWRITE_FONT_FACE_TYPE_TYPE1;
}

/*
 * Check if the passed font has a specific unicode character.
 */
static bool check_glyph(void *data, uint32_t code)
{
    HRESULT hr = S_OK;
    FontPrivate *priv = (FontPrivate *) data;
    BOOL exists = FALSE;

    if (code == 0)
        return true;

    hr = priv->font->HasCharacter(code, &exists);
    if (FAILED(hr))
        return false;

    return exists;
}

/*
 * This will release the directwrite backend
 */
static void destroy_provider(void *priv)
{
    ProviderPrivate *provider_priv = (ProviderPrivate *)priv;
    provider_priv->factory->Release();
    FreeLibrary(provider_priv->directwrite_lib);
    free(provider_priv);
}

/*
 * This will destroy a specific font and it's
 * Fontstream (in case it does exist)
 */

static void destroy_font(void *data)
{
    FontPrivate *priv = (FontPrivate *) data;

    priv->font->Release();
    if (priv->face != NULL)
      priv->face->Release();
    if (priv->stream != NULL)
      priv->stream->Release();

    free(priv);
}

static int encode_utf16(wchar_t *chars, uint32_t codepoint)
{
    if (codepoint < 0x10000) {
        chars[0] = codepoint;
        return 1;
    } else {
        chars[0] = (codepoint >> 10) + 0xD7C0;
        chars[1] = (codepoint & 0x3FF) + 0xDC00;
        return 2;
    }
}

static char *get_fallback(void *priv, const char *base, uint32_t codepoint)
{
    HRESULT hr;
    ProviderPrivate *provider_priv = (ProviderPrivate *)priv;
    struct IDWriteFactory *dw_factory = provider_priv->factory;
    struct IDWriteTextFormat *text_format = NULL;
    struct IDWriteTextLayout *text_layout = NULL;
    FallbackLogTextRenderer renderer;

    init_FallbackLogTextRenderer(&renderer, dw_factory);

    hr = dw_factory->CreateTextFormat(FALLBACK_DEFAULT_FONT, NULL,
            DWRITE_FONT_WEIGHT_MEDIUM, DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_STRETCH_NORMAL, 1.0f, L"", &text_format);
    if (FAILED(hr)) {
        return NULL;
    }

    // Encode codepoint as UTF-16
    wchar_t char_string[2];
    int char_len = encode_utf16(char_string, codepoint);

    // Create a text_layout, a high-level text rendering facility, using
    // the given codepoint and dummy format.
    hr = dw_factory->CreateTextLayout(char_string, char_len, text_format,
        0.0f, 0.0f, &text_layout);
    if (FAILED(hr)) {
      text_format->Release();
        return NULL;
    }

    // Draw the layout with a dummy renderer, which logs the
    // font used and stores it.
    struct IDWriteFont *font = NULL;
    hr = text_layout->Draw(&font, &renderer, 0.0f, 0.0f);
    // We're done with these now
    text_layout->Release();
    text_format->Release();
    if (FAILED(hr) || font == NULL) {
        return NULL;
    }


    // Now, just extract the first family name
    BOOL exists = FALSE;
    struct IDWriteLocalizedStrings *familyNames = NULL;
    hr = font->GetInformationalStrings(
            DWRITE_INFORMATIONAL_STRING_WIN32_FAMILY_NAMES,
            &familyNames, &exists);
    if (FAILED(hr) || !exists) {
      font->Release();
        return NULL;
    }

    wchar_t temp_name[NAME_MAX_LENGTH];
    hr = familyNames->GetString(0, temp_name, NAME_MAX_LENGTH);
    if (FAILED(hr)) {
      familyNames->Release();
      font->Release();
        return NULL;
    }
    temp_name[NAME_MAX_LENGTH-1] = 0;

    // DirectWrite may not have found a valid fallback, so check that
    // the selected font actually has the requested glyph.
    if (codepoint > 0) {
      hr = font->HasCharacter(codepoint, &exists);
        if (FAILED(hr) || !exists) {
          familyNames->Release();
          font->Release();
            return NULL;
        }
    }

    int size_needed = WideCharToMultiByte(CP_UTF8, 0, temp_name, -1, NULL, 0,NULL, NULL);
    char *family = (char *) malloc(size_needed);
    WideCharToMultiByte(CP_UTF8, 0, temp_name, -1, family, size_needed, NULL, NULL);

    familyNames->Release();
    font->Release();
    return family;
}

static int map_width(enum DWRITE_FONT_STRETCH stretch)
{
    switch (stretch) {
    case DWRITE_FONT_STRETCH_ULTRA_CONDENSED: return 50;
    case DWRITE_FONT_STRETCH_EXTRA_CONDENSED: return 63;
    case DWRITE_FONT_STRETCH_CONDENSED:       return FONT_WIDTH_CONDENSED;
    case DWRITE_FONT_STRETCH_SEMI_CONDENSED:  return 88;
    case DWRITE_FONT_STRETCH_MEDIUM:          return FONT_WIDTH_NORMAL;
    case DWRITE_FONT_STRETCH_SEMI_EXPANDED:   return 113;
    case DWRITE_FONT_STRETCH_EXPANDED:        return FONT_WIDTH_EXPANDED;
    case DWRITE_FONT_STRETCH_EXTRA_EXPANDED:  return 150;
    case DWRITE_FONT_STRETCH_ULTRA_EXPANDED:  return 200;
    default:
        return FONT_WIDTH_NORMAL;
    }
}

static void add_font(struct IDWriteFont *font, struct IDWriteFontFamily *fontFamily,
                     ASS_FontProvider *provider)
{
    HRESULT hr;
    BOOL exists;
    wchar_t temp_name[NAME_MAX_LENGTH];
    int size_needed;
    ASS_FontProviderMetaData meta = {0};

    meta.weight = font->GetWeight();
    meta.width = map_width(font->GetStretch());

    enum DWRITE_FONT_STYLE style = font->GetStyle();
    meta.slant = (style == DWRITE_FONT_STYLE_NORMAL) ? FONT_SLANT_NONE :
                 (style == DWRITE_FONT_STYLE_OBLIQUE)? FONT_SLANT_OBLIQUE :
                 (style == DWRITE_FONT_STYLE_ITALIC) ? FONT_SLANT_ITALIC : FONT_SLANT_NONE;

    struct IDWriteLocalizedStrings *psNames;
    hr = font->GetInformationalStrings(
            DWRITE_INFORMATIONAL_STRING_POSTSCRIPT_NAME, &psNames, &exists);
    if (FAILED(hr))
        goto cleanup;

    if (exists) {
      hr = psNames->GetString(0, temp_name, NAME_MAX_LENGTH);
        if (FAILED(hr)) {
          psNames->Release();
            goto cleanup;
        }

        temp_name[NAME_MAX_LENGTH-1] = 0;
        size_needed = WideCharToMultiByte(CP_UTF8, 0, temp_name, -1, NULL, 0, NULL, NULL);
        char *mbName = (char *) malloc(size_needed);
        if (!mbName) {
          psNames->Release();
            goto cleanup;
        }
        WideCharToMultiByte(CP_UTF8, 0, temp_name, -1, mbName, size_needed, NULL, NULL);
        meta.postscript_name = mbName;

        psNames->Release();
    }

    struct IDWriteLocalizedStrings *fontNames;
    hr = font->GetInformationalStrings(
            DWRITE_INFORMATIONAL_STRING_FULL_NAME, &fontNames, &exists);
    if (FAILED(hr))
        goto cleanup;

    if (exists) {
      meta.n_fullname = fontNames->GetCount();
        meta.fullnames = (char **) calloc(meta.n_fullname, sizeof(char *));
        if (!meta.fullnames) {
          fontNames->Release();
            goto cleanup;
        }
        for (int k = 0; k < meta.n_fullname; k++) {
          hr = fontNames->GetString(k, temp_name, NAME_MAX_LENGTH);
            if (FAILED(hr)) {
              fontNames->Release();
                goto cleanup;
            }

            temp_name[NAME_MAX_LENGTH-1] = 0;
            size_needed = WideCharToMultiByte(CP_UTF8, 0, temp_name, -1, NULL, 0, NULL, NULL);
            char *mbName = (char *) malloc(size_needed);
            if (!mbName) {
              fontNames->Release();
                goto cleanup;
            }
            WideCharToMultiByte(CP_UTF8, 0, temp_name, -1, mbName, size_needed, NULL, NULL);
            meta.fullnames[k] = mbName;
        }
        fontNames->Release();
    }

    struct IDWriteLocalizedStrings *familyNames;
    hr = font->GetInformationalStrings(
            DWRITE_INFORMATIONAL_STRING_WIN32_FAMILY_NAMES, &familyNames, &exists);
    if (FAILED(hr) || !exists)
      hr = fontFamily->GetFamilyNames(&familyNames);
    if (FAILED(hr))
        goto cleanup;

    meta.n_family = familyNames->GetCount();
    meta.families = (char **) calloc(meta.n_family, sizeof(char *));
    if (!meta.families) {
      familyNames->Release();
        goto cleanup;
    }
    for (int k = 0; k < meta.n_family; k++) {
      hr = familyNames->GetString(k, temp_name, NAME_MAX_LENGTH);
        if (FAILED(hr)) {
          familyNames->Release();
            goto cleanup;
        }

        temp_name[NAME_MAX_LENGTH-1] = 0;
        size_needed = WideCharToMultiByte(CP_UTF8, 0, temp_name, -1, NULL, 0, NULL, NULL);
        char *mbName = (char *) malloc(size_needed);
        if (!mbName) {
          familyNames->Release();
            goto cleanup;
        }
        WideCharToMultiByte(CP_UTF8, 0, temp_name, -1, mbName, size_needed, NULL, NULL);
        meta.families[k] = mbName;
    }
    familyNames->Release();

    FontPrivate *font_priv = (FontPrivate *) calloc(1, sizeof(*font_priv));
    if (!font_priv)
        goto cleanup;
    font_priv->font = font;
    font = NULL;

    ass_font_provider_add_font(provider, &meta, NULL, 0, font_priv);

cleanup:
    if (meta.families) {
        for (int k = 0; k < meta.n_family; k++)
            free(meta.families[k]);
        free(meta.families);
    }

    if (meta.fullnames) {
        for (int k = 0; k < meta.n_fullname; k++)
            free(meta.fullnames[k]);
        free(meta.fullnames);
    }

    free(meta.postscript_name);

    if (font)
      font->Release();
}

/*
 * Scan every system font on the current machine and add it
 * to the libass lookup. Stores the FontPrivate as private data
 * for later memory reading
 */
static void scan_fonts(struct IDWriteFactory *factory,
                       ASS_FontProvider *provider)
{
    HRESULT hr = S_OK;
    struct IDWriteFontCollection *fontCollection = NULL;
    struct IDWriteFont *font = NULL;
    hr = factory->GetSystemFontCollection(&fontCollection, FALSE);

    if (FAILED(hr) || !fontCollection)
        return;

    UINT32 familyCount = fontCollection->GetFontFamilyCount();

    for (UINT32 i = 0; i < familyCount; ++i) {
        struct IDWriteFontFamily *fontFamily = NULL;

        hr = fontCollection->GetFontFamily(i, &fontFamily);
        if (FAILED(hr))
            continue;

        UINT32 fontCount = fontFamily->GetFontCount();
        for (UINT32 j = 0; j < fontCount; ++j) {
          hr = fontFamily->GetFont(j, &font);
            if (FAILED(hr))
                continue;

            // Simulations for bold or oblique are sometimes synthesized by
            // DirectWrite. We are only interested in physical fonts.
            if (font->GetSimulations() != 0) {
              font->Release();
                continue;
            }

            add_font(font, fontFamily, provider);
        }

        fontFamily->Release();
    }

    fontCollection->Release();
}

static void get_substitutions(void *priv, const char *name,
                              ASS_FontProviderMetaData *meta)
{
    const int n = sizeof(font_substitutions) / sizeof(font_substitutions[0]);
    ass_map_font(font_substitutions, n, name, meta);
}

/*
 * Called by libass when the provider should perform the
 * specified task
 */
static ASS_FontProviderFuncs directwrite_callbacks = {
    get_data,
    check_postscript,
    check_glyph,
    destroy_font,
    destroy_provider,
    nullptr,
    get_substitutions,
    get_fallback,
};

/*
 * Register the directwrite provider. Upon registering
 * scans all system fonts. The private data for this
 * provider is IDWriteFactory
 * On failure returns NULL
 */
ASS_FontProvider *ass_directwrite_add_provider(ASS_Library *lib,
                                               ASS_FontSelector *selector,
                                               const char *config)
{
    HRESULT hr = S_OK;
    struct IDWriteFactory *dwFactory = NULL;
    ASS_FontProvider *provider = NULL;
    ProviderPrivate *priv = NULL;

    hr = DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED,
                                __uuidof(IDWriteFactory),
                                (IUnknown **) (&dwFactory));
    if (FAILED(hr) || !dwFactory) {
        ass_msg(lib, MSGL_WARN, "Failed to initialize directwrite.");
        dwFactory = NULL;
        goto cleanup;
    }

    priv = (ProviderPrivate *)calloc(sizeof(*priv), 1);
    if (!priv)
        goto cleanup;

    priv->factory = dwFactory;
    provider = ass_font_provider_new(selector, &directwrite_callbacks, priv);
    if (!provider)
        goto cleanup;

    scan_fonts(dwFactory, provider);
    return provider;

cleanup:

    free(priv);
    if (dwFactory)
        dwFactory->Release();

    return NULL;
}
