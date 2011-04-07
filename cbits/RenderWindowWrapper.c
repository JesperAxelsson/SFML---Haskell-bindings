
#ifndef SFML_RENDERWINDOW_WRAPPER
#define SFML_RENDERWINDOW_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/RenderWindow.h>

CSFML_API sfRenderWindow *sfRenderWindow_CreateWrapper(sfVideoMode *mode, const char *title, unsigned long style, const sfContextSettings *settings)
{
  return sfRenderWindow_Create(*mode, title, style, settings);
}

CSFML_API void sfRenderWindow_GetSettingsWrapper(sfRenderWindow *window, sfContextSettings *settings)
{
  *settings = sfRenderWindow_GetSettings(window);
}

CSFML_API void sfRenderWindow_ClearWrapper(sfRenderWindow *window, sfColor *color)
{
  sfRenderWindow_Clear(window, *color);
}

CSFML_API void sfRenderWindow_GetViewportWrapper(const sfRenderWindow* renderWindow, const sfView* view, sfIntRect *viewport)
{
  *viewport = sfRenderWindow_GetViewport(renderWindow, view);
}

#endif // SFML_RENDERWINDOW_WRAPPER
