#ifndef SFML_FONT_WRAPPER
#define SFML_FONT_WRAPPER

#include <SFML/Config.h>
#include <SFML/Graphics/Font.h>

CSFML_API void sfFont_GetGlyphWrapper(sfFont *font, sfUint32 codePoint, unsigned int characterSize, sfBool bold, sfGlyph *glyph)
{
  *glyph = sfFont_GetGlyph(font, codePoint, characterSize, bold);
}

#endif // SFML_FONT_WRAPPER
