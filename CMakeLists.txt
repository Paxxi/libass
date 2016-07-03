cmake_minimum_required(VERSION 3.2)

project(libass LANGUAGES C)

find_package(iconv)
find_package(freetype 2.6 REQUIRED NO_MODULE)
find_package(fribidi REQUIRED NO_MODULE)

add_library(libass
  libass/ass_bitmap.c
  libass/ass_bitmap.h
  libass/ass_blur.c
  libass/ass_cache.c
  libass/ass_cache.h
  libass/ass_cache_template.h
  libass/ass_compat.h
  libass/ass_drawing.c
  libass/ass_drawing.h
  libass/ass_font.c
  libass/ass_font.h
  libass/ass_fontselect.c
  libass/ass_fontselect.h
  libass/ass_func_template.h
  libass/ass_library.c
  libass/ass_library.h
  libass/ass_parse.c
  libass/ass_parse.h
  libass/ass_rasterizer.c
  libass/ass_rasterizer.h
  libass/ass_rasterizer_c.c
  libass/ass_render.c
  libass/ass_render.h
  libass/ass_render_api.c
  libass/ass_shaper.c
  libass/ass_shaper.h
  libass/ass_string.c
  libass/ass_string.h
  libass/ass_strtod.c
  libass/ass_types.h
  libass/ass_utils.c
  libass/ass_utils.h
  libass/ass.c
  libass/ass.h
  )

if(iconv_VERSION)
  target_link_libraries(libass IMPORTED iconv::iconv)
  # target_include_directories(libass IMPORTED iconv::iconv)
  target_compile_definitions(libass PRIVATE CONFIG_ICONV)
endif()

target_link_libraries(libass IMPORTED
  freetype::freetype
  fribidi::fribidi
)
target_compile_definitions(libass PRIVATE CONFIG_RASTERIZER)



if(MSVC)
  target_sources(libass PRIVATE
    libass/ass_directwrite.h
    libass/ass_directwrite.c
  )
set(CMAKE_DEBUG_POSTFIX "d")
endif()