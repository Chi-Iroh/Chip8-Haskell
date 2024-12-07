module Either where

fromLeft' :: Either a b -> a
fromLeft' (Left a) = a
fromLeft' _ = error "Nothing at Right !"

fromRight' :: Either a b -> b
fromRight' (Right b) = b
fromRight' _ = error "Nothing at Left !"

maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a) = Just a
maybeLeft _ = Nothing

maybeRight :: Either a b -> Maybe b
maybeRight (Right b) = Just b
maybeRight _ = Nothing
