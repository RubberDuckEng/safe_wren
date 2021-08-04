workspace "wren"
  configurations { "Release", "Debug" }
  platforms { "64bit", "32bit" }
  defaultplatform "64bit"
  startproject "wren_test"

  filter "configurations:Debug"
    targetsuffix "_d"
    defines { "DEBUG" }
    symbols "On"

  filter "configurations:Release"
    defines { "NDEBUG" }
    optimize "On"

project "wren_test"
  kind "ConsoleApp"
  language "C"
  cdialect "C99"
  targetdir "bin"
  links { "wren_rust" }
  libdirs { "../target/debug" }

  prebuildcommands { "cargo build" }

  files {
    "main.c",
    "test.c",
    "test.h",
    "api/*.c",
    "api/*.h"
  }

  includedirs {
    "../wren_c/src/include"
  }
