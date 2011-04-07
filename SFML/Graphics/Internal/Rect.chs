{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SFML.Graphics.Internal.Rect where

#include <SFML/Graphics/Rect.h>

{#import SFML.Graphics.Internal.Types #}

import Foreign (unsafePerformIO)
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal
import SFML.ForeignUtils

{#context lib="csfml-graphics" prefix="sf" #}

instance Storable (Rect Float) where
  sizeOf _ = {#sizeof FloatRect #}
  alignment _ = {#alignof FloatRect #}
  peek ptr = do
    left <- fmap realToFrac $ {#get FloatRect.Left#} ptr
    top <- fmap realToFrac $ {#get FloatRect.Top#} ptr
    width <- fmap realToFrac $ {#get FloatRect.Width#} ptr
    height <- fmap realToFrac $ {#get FloatRect.Height#} ptr
    return (Rect left top width height)
  poke ptr (Rect left top width height) = do
    {#set FloatRect.Left #} ptr (realToFrac left)
    {#set FloatRect.Top #} ptr (realToFrac top)
    {#set FloatRect.Width #} ptr (realToFrac width)
    {#set FloatRect.Height #} ptr (realToFrac height)

instance Storable (Rect Int) where
  sizeOf _ = {#sizeof IntRect #}
  alignment _ = {#alignof IntRect #}
  peek ptr = do
    left <- fmap fromIntegral $ {#get IntRect.Left#} ptr
    top <- fmap fromIntegral $ {#get IntRect.Top#} ptr
    width <- fmap fromIntegral $ {#get IntRect.Width#} ptr
    height <- fmap fromIntegral $ {#get IntRect.Height#} ptr
    return (Rect left top width height)
  poke ptr (Rect left top width height) = do
    {#set IntRect.Left #} ptr (fromIntegral left)
    {#set IntRect.Top #} ptr (fromIntegral top)
    {#set IntRect.Width #} ptr (fromIntegral width)
    {#set IntRect.Height #} ptr (fromIntegral height)

class Rectable a where
  rectContains :: Rect a -> a -> a -> Bool
  rectIntersection :: Rect a -> Rect a -> Maybe (Rect a)

instance Rectable Float where
  rectContains rect x y = unsafePerformIO $
                          withT rect $ \rectPtr ->
                          {#call unsafe FloatRect_Contains#} (castPtr rectPtr) (realToFrac x) (realToFrac y) >>= \b ->
                          return (toBool b)
  rectIntersection rect1 rect2 = unsafePerformIO $
                                 withT rect1 $ \rect1Ptr ->
                                 withT rect2 $ \rect2Ptr ->
                                 alloca $ \outRectPtr -> do
                                 intersects <- {#call unsafe FloatRect_Intersects#} (castPtr rect1Ptr) (castPtr rect2Ptr) (castPtr outRectPtr)
                                 if toBool intersects
                                   then fmap Just (peek outRectPtr)
                                   else return Nothing

instance Rectable Int where
  rectContains rect x y = unsafePerformIO $
                          withT rect $ \rectPtr ->
                          {#call unsafe IntRect_Contains#} (castPtr rectPtr) (fromIntegral x) (fromIntegral y) >>= \b ->
                          return (toBool b)
  rectIntersection rect1 rect2 = unsafePerformIO $
                                 withT rect1 $ \rect1Ptr ->
                                 withT rect2 $ \rect2Ptr ->
                                 alloca $ \outRectPtr -> do
                                 intersects <- {#call unsafe IntRect_Intersects#} (castPtr rect1Ptr) (castPtr rect2Ptr) (castPtr outRectPtr)
                                 if toBool intersects
                                   then fmap Just (peek outRectPtr)
                                   else return Nothing

peekRect :: Storable (Rect a) => Ptr () -> IO (Rect a)
peekRect ptr = peek (castPtr ptr)

allocaIntRect :: (Ptr () -> IO b) -> IO b
allocaIntRect a = alloca $ \(ptr :: Ptr (Rect Int)) -> a (castPtr ptr)

allocaFloatRect :: (Ptr () -> IO b) -> IO b
allocaFloatRect a = alloca $ \(ptr :: Ptr (Rect Float)) -> a (castPtr ptr)
