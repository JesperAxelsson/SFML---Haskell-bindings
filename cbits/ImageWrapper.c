
#ifndef SFML_IMAGE_WRAPPER
#define SFML_IMAGE_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/Image.h>

CSFML_API sfImage *sfImage_CreateFromColorWrapper(unsigned int width, unsigned int height, sfColor *color)
{
  return sfImage_CreateFromColor(width, height, *color);
}

CSFML_API void sfImage_CreateMaskFromColorWrapper(sfImage *image, sfColor *colorKey, sfUint8 alpha)
{
  sfImage_CreateMaskFromColor(image, *colorKey, alpha);
}

CSFML_API void sfImage_CopyImageWrapper(sfImage *image, sfImage *source, unsigned int destX, unsigned int destY, sfIntRect *sourceRect)
{
  sfImage_CopyImage(image, source, destX, destY, *sourceRect);
}

CSFML_API sfBool sfImage_CopyScreenWrapper(sfImage *image, sfRenderWindow *window, sfIntRect *sourceRect)
{
  return sfImage_CopyScreen(image, window, *sourceRect);
}

CSFML_API void sfImage_SetPixelWrapper(sfImage *image, unsigned int x, unsigned int y, sfColor *color)
{
  sfImage_SetPixel(image, x, y, *color);
}

CSFML_API void sfImage_GetPixelWrapper(sfImage *image, unsigned int x, unsigned int y, sfColor *outColor)
{
  *outColor = sfImage_GetPixel(image, x, y);
}

CSFML_API void sfImage_UpdatePixelsWrapper(sfImage *image, const sfUint8 *pixels, sfIntRect *rectangle)
{
  sfImage_UpdatePixels(image, pixels, *rectangle);
}

#endif // SFML_IMAGE_WRAPPER
