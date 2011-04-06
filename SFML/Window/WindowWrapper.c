#ifndef SFML_WINDOW_WRAPPER
#define SFML_WINDOW_WRAPPER

#include <SFML/Config.h>
#include <SFML/Window/Window.h>

CSFML_API sfWindow *sfWindow_CreateWrapper(sfVideoMode *mode, const char *title, unsigned long style, const sfContextSettings *settings)
{
  return sfWindow_Create(*mode, title, style, settings);
}

CSFML_API sfWindow *sfWindow_CreateWrapperSimple(sfVideoMode *mode, const char *title, unsigned long style)
{
  return sfWindow_Create(*mode, title, style, NULL);
}

CSFML_API sfContextSettings *sfWindow_GetSettingsWrapper(sfWindow *window)
{
  sfContextSettings *settings = malloc(sizeof(sfContextSettings));
  *settings = sfWindow_GetSettings(window);
  return settings;
}

#endif // SFML_WINDOW_WRAPPER
