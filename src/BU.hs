module BU
( PrettyShow
, prettyShow
) where


class PrettyShow p where
  prettyShow :: p -> String
