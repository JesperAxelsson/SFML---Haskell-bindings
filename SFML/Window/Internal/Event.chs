{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Window.Internal.Event where

#include <SFML/Window/Event.h>

{#import SFML.Window.Internal.Types #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Utils
import Data.Word
import Data.Char

{#context lib="csfml-window" prefix="sf" #}

instance Storable Event where
  sizeOf _ = {#sizeof Event #}
  alignment _ = {#alignof Event #}
  peek ptr = do
    t <- fmap cToEnum $ {#get Event.Type #} ptr
    case t of
      EvtClosed -> return Closed
      EvtResized -> peekSizeEvent ptr
      EvtLostFocus -> return LostFocus
      EvtGainedFocus -> return GainedFocus
      EvtTextEntered -> peekTextEvent ptr
      EvtKeyPressed -> peekKeyEvent KeyPressed ptr
      EvtKeyReleased -> peekKeyEvent KeyReleased ptr
      EvtMouseWheelMoved -> peekMouseWheelEvent ptr
      EvtMouseButtonPressed -> peekMouseButtonEvent MouseButtonPressed ptr
      EvtMouseButtonReleased -> peekMouseButtonEvent MouseButtonReleased ptr
      EvtMouseMoved -> peekMouseMoveEvent ptr
      EvtMouseEntered -> return MouseEntered
      EvtMouseLeft -> return MouseLeft
      EvtJoyButtonPressed -> peekJoyButtonEvent JoyButtonPressed ptr
      EvtJoyButtonReleased -> peekJoyButtonEvent JoyButtonReleased ptr
      EvtJoyMoved -> peekJoyMoveEvent ptr
  poke ptr evt = case evt of
    Closed{} -> pokeEvt ptr EvtClosed
    Resized{} -> pokeEvt ptr EvtResized >> pokeSizeEvent ptr evt
    LostFocus{} -> pokeEvt ptr EvtLostFocus
    GainedFocus{} -> pokeEvt ptr EvtGainedFocus
    TextEntered{} -> pokeEvt ptr EvtTextEntered >> pokeTextEvent ptr evt
    KeyPressed{} -> pokeEvt ptr EvtKeyPressed >> pokeKeyEvent ptr evt
    KeyReleased{} -> pokeEvt ptr EvtKeyReleased >> pokeKeyEvent ptr evt
    MouseWheelMoved{} -> pokeEvt ptr EvtMouseWheelMoved >> pokeMouseMoveEvent ptr evt
    MouseButtonPressed{} -> pokeEvt ptr EvtMouseButtonPressed >> pokeMouseButtonEvent ptr evt
    MouseButtonReleased{} -> pokeEvt ptr EvtMouseButtonReleased >> pokeMouseButtonEvent ptr evt
    MouseMoved{} -> pokeEvt ptr EvtMouseMoved >> pokeMouseMoveEvent ptr evt
    MouseEntered{} -> pokeEvt ptr EvtMouseEntered
    MouseLeft{} -> pokeEvt ptr EvtMouseLeft
    JoyButtonPressed{} -> pokeEvt ptr EvtJoyButtonPressed >> pokeJoyButtonEvent ptr evt
    JoyButtonReleased{} -> pokeEvt ptr EvtJoyButtonReleased >> pokeJoyButtonEvent ptr evt
    JoyMoved{} -> pokeEvt ptr EvtJoyMoved >> pokeJoyMoveEvent ptr evt

peekEvt ptr = do
  fmap cToEnum $ {#get Event.Type #} ptr
pokeEvt ptr evt = {#set Event.Type #} ptr (cFromEnum evt)


peekKeyEvent constr ptr = do
  code <- fmap cToEnum $ {#get Event.Key.Code #} ptr
  alt <- fmap toBool $ {#get Event.Key.Alt #} ptr
  ctrl <- fmap toBool $ {#get Event.Key.Control #} ptr
  shift <- fmap toBool $ {#get Event.Key.Shift #} ptr
  return (constr code alt ctrl shift)
-- (KeyEvent code alt ctrl shift)
pokeKeyEvent ptr evt = do
  {#set Event.Key.Code #} ptr (cFromEnum (code evt))
  {#set Event.Key.Alt #} ptr (fromBool (alt evt))
  {#set Event.Key.Control #} ptr (fromBool (control evt))
  {#set Event.Key.Shift #} ptr (fromBool (shift evt))

peekTextEvent ptr = fmap (TextEntered . chr . fromIntegral) $ {#get Event.Text.Unicode #} ptr
pokeTextEvent ptr (TextEntered char) = {#set Event.Text.Unicode #} ptr (fromIntegral . ord $ char)

peekMouseMoveEvent ptr = do
  x <- fmap fromIntegral $ {#get Event.MouseMove.X #} ptr
  y <- fmap fromIntegral $ {#get Event.MouseMove.Y #} ptr
  return (MouseMoved x y)
pokeMouseMoveEvent ptr (MouseMoved x y) = do
  {#set Event.MouseMove.X #} ptr (fromIntegral x)
  {#set Event.MouseMove.Y #} ptr (fromIntegral y)

peekMouseButtonEvent constr ptr = do
  b <- fmap cToEnum $ {#get Event.MouseButton.Button #} ptr
  x <- fmap fromIntegral $ {#get Event.MouseButton.X #} ptr
  y <- fmap fromIntegral $ {#get Event.MouseButton.Y #} ptr
  return (constr b x y)
-- (MouseButtonEvent b x y)
pokeMouseButtonEvent ptr evt = do
  {#set Event.MouseButton.Button #} ptr (cFromEnum (button evt))
  {#set Event.MouseButton.X #} ptr (fromIntegral (x evt))
  {#set Event.MouseButton.Y #} ptr (fromIntegral (y evt))

peekMouseWheelEvent ptr = do
  d <- fmap fromIntegral $ {#get Event.MouseWheel.Delta #} ptr
  x <- fmap fromIntegral $ {#get Event.MouseWheel.X #} ptr
  y <- fmap fromIntegral $ {#get Event.MouseWheel.Y #} ptr
  return (MouseWheelMoved d x y)
pokeMouseWheelEvent ptr (MouseWheelMoved d x y) = do
  {#set Event.MouseWheel.Delta #} ptr (fromIntegral d)
  {#set Event.MouseWheel.X #} ptr (fromIntegral x)
  {#set Event.MouseWheel.Y #} ptr (fromIntegral y)

peekJoyMoveEvent ptr = do
  id <- fmap fromIntegral $ {#get Event.JoyMove.JoystickId #} ptr
  a <- fmap cToEnum $ {#get Event.JoyMove.Axis #} ptr
  p <- fmap realToFrac $ {#get Event.JoyMove.Position #} ptr
  return (JoyMoved id a p)
pokeJoyMoveEvent ptr (JoyMoved id a p) = do
  {#set Event.JoyMove.JoystickId #} ptr (fromIntegral id)
  {#set Event.JoyMove.Axis #} ptr (cFromEnum a)
  {#set Event.JoyMove.Position #} ptr (realToFrac p)

peekJoyButtonEvent constr ptr = do
  id <- fmap fromIntegral $ {#get Event.JoyButton.JoystickId #} ptr
  b <- fmap fromIntegral $ {#get Event.JoyButton.Button #} ptr
  return (constr id b)
-- (JoyMoveEvent id b)
pokeJoyButtonEvent ptr evt = do
  {#set Event.JoyButton.JoystickId #} ptr (fromIntegral (joyId evt))
  {#set Event.JoyButton.Button #} ptr (fromIntegral (joyButton evt))

peekSizeEvent ptr = do
  w <- fmap fromIntegral $ {#get Event.Size.Width #} ptr
  h <- fmap fromIntegral $ {#get Event.Size.Height #} ptr
  return (Resized w h)
pokeSizeEvent ptr (Resized w h) = do
  {#set Event.Size.Width #} ptr (fromIntegral w)
  {#set Event.Size.Height #} ptr (fromIntegral h)
