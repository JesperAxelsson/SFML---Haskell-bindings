#ifndef SFML_RENDERIMAGE_WRAPPER
#define SFML_RENDERIMAGE_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/RenderImage.h>

CSFML_API void sfRenderImage_ClearWrapper(sfRenderImage* renderImage, sfColor *color)
{
  sfRenderImage_Clear(renderImage, *color);
}

CSFML_API void sfRenderImage_GetViewportWrapper(const sfRenderImage* renderImage, const sfView* view, sfIntRect *rect)
{
  *rect = sfRenderImage_GetViewport(renderImage, view);
}

#endif // SFML_RENDERIMAGE_WRAPPER
