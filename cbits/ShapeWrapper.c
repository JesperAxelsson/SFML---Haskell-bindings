#ifndef SFML_SHAPE_WRAPPER
#define SFML_SHAPE_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/Shape.h>

CSFML_API sfShape* sfShape_CreateLineWrapper(float p1x, float p1y, float p2x, float p2y, float thickness, sfColor *color, float outline, sfColor *outlineColor)
{
  return sfShape_CreateLine(p1x, p1y, p2x, p2y, thickness, *color, outline *outlineColor);
}

CSFML_API sfShape* sfShape_CreateRectangleWrapper(float left, float top, float width, float height, sfColor *color, float outline, sfColor *outlineColor)
{
  return sfShape_CreateRectangle(left, top, width, height, *color, outline, *outlineColor);
}

CSFML_API sfShape* sfShape_CreateCircleWrapper(float x, float y, float radius, sfColor *color, float outline, sfColor *outlineColor)
{
  return sfShape_CreateCircle(x, y, radius, *color, outline, *outlineColor);
}

CSFML_API void sfShape_SetColorWrapper(sfShape* shape, sfColor *color)
{
  return sfShape_SetColor(shape, *color);
}

CSFML_API void sfShape_GetColorWrapper(const sfShape* shape, sfColor *color)
{
  *color = sfShape_GetColor(shape);
}

CSFML_API void sfShape_AddPointWrapper(sfShape* shape, float x, float y, sfColor *color, sfColor *outlineColor)
{
  sfShape_AddPoint(shape, x, y, *color, *outlineColor);
}

CSFML_API void sfShape_GetPointColorWrapper(const sfShape* shape, unsigned int index, sfColor *color)
{
  *color = sfShape_GetPointColor(shape);
}

CSFML_API void sfShape_GetPointOutlineColorWrapper(const sfShape* shape, unsigned int index, sfColor *color)
{
  *color = sfShape_GetPointOutlineColor(shape);
}

CSFML_API void sfShape_GetColorWrapper(const sfShape* shape, sfColor *color)
{
  *color = sfShape_GetColor(shape);
}

CSFML_API void sfShape_SetPointColorWrapper(sfShape* shape, unsigned int index, sfColor *color)
{
  sfShape_SetPointColor(shape, index, *color);
}

CSFML_API void sfShape_SetPointOutlineColorWrapper(sfShape* shape, unsigned int index, sfColor *color)
{
  sfShape_SetPointOutlineColor(shape, index, *color);
}


#endif // SFML_SHAPE_WRAPPER
