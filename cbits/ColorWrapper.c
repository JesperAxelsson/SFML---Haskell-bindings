#ifndef SFML_COLOR_WRAPPER
#define SFML_COLOR_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/Color.h>

CSFML_API void sfColor_FromRGBWrapper(sfUint8 red, sfUint8 green, sfUint8 blue, sfColor *color)
{
  *color = sfColor_FromRGB(red, green, blue);
}

CSFML_API void sfColor_FromRGBAWrapper(sfUint8 red, sfUint8 green, sfUint8 blue, sfUint8 alpha, sfColor *color)
{
  *color = sfColor_FromRGBA(red, green, blue, alpha);
}

CSFML_API void sfColor_AddWrapper(sfColor *color1, sfColor *color2, sfColor *colorOut)
{
  *colorOut = sfColor_Add(*color1, *color2);
}

CSFML_API void sfColor_ModulateWrapper(sfColor *color1, sfColor *color2, sfColor *colorOut)
{
  *colorOut = sfColor_Modulate(*color1, *color2);
}

#endif // SFML_COLOR_WRAPPER
