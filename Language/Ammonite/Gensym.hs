module Language.Ammonite.Gensym
	( GensymSource
	, Gensym
	, startSource
	, splitSource
	, step
	, extern
	) where

newtype GensymSource = GS [Integer]
newtype Gensym = G [Integer]
	deriving (Eq)

startSource :: GensymSource
startSource = GS [0]

splitSource :: GensymSource -> (GensymSource, GensymSource)
splitSource (GS (top:rest)) = (GS (top+1:rest), GS (0:top:rest))

step :: GensymSource -> (Gensym, GensymSource)
step (GS (top:rest)) = (G (top:rest), GS (top+1:rest))

-- only to distinguish one gensym from another when showing dependent data structures
extern :: Gensym -> [Integer]
extern (G x) = x