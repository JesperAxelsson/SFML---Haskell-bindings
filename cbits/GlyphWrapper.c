#ifndef SFML_GLYPH_WRAPPER
#define SFML_GLYPH_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/Glyph.h>

CSFML_API void sfGlyph_GetBounds(sfGlyph *glyph, sfIntRect *bounds)
{
  *bounds = glyph->Bounds;
}

CSFML_API void sfGlyph_GetSubRect(sfGlyph *glyph, sfIntRect *subRect)
{
  *subRect = glyph->SubRect;
}

CSFML_API void sfGlyph_SetBounds(sfGlyph *glyph, sfIntRect *bounds)
{
  glyph->Bounds = *bounds;
}

CSFML_API void sfGlyph_SetSubRect(sfGlyph *glyph, sfIntRect *subRect)
{
  glyph->SubRect = *subRect;
}

#endif // SFML_GLYPH_WRAPPER
