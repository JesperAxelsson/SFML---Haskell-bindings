
#ifndef SFML_SPRITE_WRAPPER
#define SFML_SPRITE_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/Sprite.h>

CSFML_API void sfSprite_SetColorWrapper(sfSprite* sprite, sfColor *color)
{
  sfSprite_SetColor(sprite, *color);
}

CSFML_API void sfSprite_GetColorWrapper(const sfSprite* sprite, sfColor *color)
{
  *color = sfSprite_GetColor(sprite);
}

CSFML_API void sfSprite_SetSubRectWrapper(sfSprite* sprite, sfIntRect *rectangle)
{
  sfSprite_SetSubRect(sprite, *rectangle);
}

CSFML_API void sfSprite_GetSubRectWrapper(const sfSprite* sprite, sfIntRect *rect)
{
  *rect = sfSprite_GetSubRect(sprite);
}

CSFML_API void sfSprite_GetPixelWrapper(const sfSprite* sprite, unsigned int x, unsigned int y, sfColor *color)
{
  *color = sfSprite_GetPixel(sprite, x, y);
}

#endif // SFML_SPRITE_WRAPPER
