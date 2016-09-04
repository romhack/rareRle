rareRle - Graphics decompression tool in SNES game "Battletoads & Double Dragon - The Ultimate Team".
=========
Tool was written for my [note](https://romhack.github.io/doc/snesTiles) regarding compressed graphics search process in general. "Battletoads & Double Dragon - The Ultimate Team" uses complex 8/16 bit RLE scheme for tiles compression.  
This tool can unpack graphics into 4bpp planar composite format. If you will need a packer for this format, please, [let me know](https://romhack.github.io/about.html) and I will try to write it for your project. 
```
Usage: rareRle [-v]| NAME OFFST
  NAME is a ROM name
  OFFSET is offset of packed tiles
  -v      show version number
```
You can try 
```
rareRle "Battletoads & Double Dragon - The Ultimate Team (U) [!].smc" 0xEF49F
```
for main font unpack.  
Source can be compiled with [Haskell stack](http://docs.haskellstack.org/en/stable/install_and_upgrade/). 

Griever (romhack.github.io)
