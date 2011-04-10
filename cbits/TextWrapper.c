
#ifndef SFML_TEXT_WRAPPER
#define SFML_TEXT_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/Text.h>

CSFML_API void sfText_SetColorWrapper(sfText* text, sfColor *color)
{
  sfText_SetColor(text, *color);
}

CSFML_API void sfText_GetColorWrapper(const sfText* text, sfColor *color)
{
  *color = sfText_GetColor(text);
}

#endif // SFML_TEXT_WRAPPER
