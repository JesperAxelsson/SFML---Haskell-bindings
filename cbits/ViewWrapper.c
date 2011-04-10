#ifndef SFML_VIEW_WRAPPER
#define SFML_VIEW_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/View.h>

CSFML_API sfView* sfView_CreateFromRectWrapper(sfFloatRect *rectangle)
{
  return sfView_CreateFromRect(*rectangle);
}

CSFML_API void sfView_SetViewportWrapper(sfView* view, sfFloatRect *viewport)
{
  sfView_SetViewport(view, *viewport);
}

CSFML_API void sfView_ResetWrapper(sfView* view, sfFloatRect *rectangle)
{
  sfView_Reset(view, *rectangle);
}

CSFML_API void sfView_GetViewportWrapper(const sfView* view, sfFloatRect *rect)
{
  *rect = sfView_GetViewport(view);
}

#endif // SFML_VIEW_WRAPPER
