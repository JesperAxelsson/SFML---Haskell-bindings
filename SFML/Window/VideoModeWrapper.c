#ifndef SFML_VIDEOMODE_WRAPPER
#define SFML_VIDEOMODE_WRAPPER

#include <SFML/Config.h>
#include <SFML/Window/VideoMode.h>

CSFML_API sfVideoMode *sfVideoMode_GetDesktopModeWrapper(sfVideoMode *ptr)
{
  *ptr = sfVideoMode_GetDesktopMode();
  return ptr;
}

CSFML_API sfBool sfVideoMode_IsValidWrapper(sfVideoMode *mode)
{
  return sfVideoMode_IsValid(*mode);
}

#endif // SFML_VIDEOMODE_WRAPPER
