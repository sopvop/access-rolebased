module Control.Access.RoleBased.Internal.Role where

------------------------------------------------------------------------------
import           Control.Monad.ST
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.String
import           Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as VA


------------------------------------------------------------------------------
data RoleValue = RoleBool Bool
               | RoleText Text
               | RoleInt Int
               | RoleDouble Double
  deriving (Ord, Eq, Show)


------------------------------------------------------------------------------
instance IsString RoleValue where
    fromString = RoleText . fromString


------------------------------------------------------------------------------
instance Hashable RoleValue where
    hashWithSalt salt (RoleBool e)   = salt `hashWithSalt` e `hashWithSalt` (1::Int)
    hashWithSalt salt (RoleText t)   = salt `hashWithSalt` t `hashWithSalt` (2::Int)
    hashWithSalt salt (RoleInt i)    = salt `hashWithSalt` (3::Int) `hashWithSalt` i
    hashWithSalt salt (RoleDouble d) =
        salt `hashWithSalt` (4::Int) `hashWithSalt` d


------------------------------------------------------------------------------
data Role = Role {
      _roleName :: Text
    , _roleData :: HashMap Text RoleValue
    }
  deriving (Eq, Show)


------------------------------------------------------------------------------
instance IsString Role where
    fromString s = Role (fromString s) M.empty


------------------------------------------------------------------------------
toSortedList :: (Ord k, Ord v) => HashMap k v -> [(k,v)]
toSortedList m = runST $ do
    v <- V.unsafeThaw $ V.fromList $ M.toList m
    VA.sort v
    v' <- V.unsafeFreeze v
    return $ V.toList v'


------------------------------------------------------------------------------
instance Hashable Role where
    hashWithSalt salt (Role nm dat) =
        h $ hashWithSalt salt nm
      where
        h s = hashWithSalt s $ toSortedList dat


------------------------------------------------------------------------------
data RoleValueMeta = RoleBoolMeta
                   | RoleTextMeta
                   | RoleEnumMeta [Text]
                   | RoleIntMeta
                   | RoleDoubleMeta


------------------------------------------------------------------------------
data RoleDataDefinition = RoleDataDefinition {
      _roleDataName        :: Text
    , _roleValueMeta       :: RoleValueMeta
    , _roleDataDescription :: Text
    }


------------------------------------------------------------------------------
data RoleMetadata = RoleMetadata {
      _roleMetadataName :: Text
    , _roleDescription  :: Text
    , _roleDataDefs     :: [RoleDataDefinition]
    }
