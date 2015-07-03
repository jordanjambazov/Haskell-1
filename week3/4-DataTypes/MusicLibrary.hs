module MusicLibrary where

type Title = String
type Name  = String
type Length = Double

data Author = Author {
  authorName :: Name
} deriving (Eq, Show)

data Genre = Genre {
  genreName :: Name
} deriving (Eq, Show)

data Song = Song {
  songTitle  :: Title,
  songAuthor :: Author,
  songGenre  :: Genre,
  songLength :: Length
} deriving (Eq, Show)

data Library = Library {
  songs :: [Song]
} deriving (Eq, Show)


addSong :: Library -> Song -> Library
addSong library song = Library (song : songs library)

removeSong :: Library -> Song -> Library
removeSong (Library [])     _                  = Library []
removeSong (Library (x:xs)) song |  x == song  = removeSong (Library xs) x
                                 |  otherwise  = addSong (removeSong (Library xs) song) x

removeSongsByAuthor :: Library -> Author -> Library
removeSongsByAuthor library author = Library $ filter (\song -> (songAuthor song) /= author) (songs library)

metallica = Author {authorName = "Metallica"}
rock = Genre {genreName = "Rock"}
one = Song {songTitle = "One", songAuthor = metallica, songGenre=rock, songLength = 169}
unforgiven = Song {songTitle = "The Unforgiven", songAuthor = metallica, songGenre = rock, songLength = 200}

joeCocker = Author {authorName = "Joe Cocker"}
youCanLeaveYourOne = Song {
  songTitle = "You can leave your hat on",
  songAuthor = joeCocker,
  songGenre=rock,
  songLength = 210
}

emptyLibrary :: Library
emptyLibrary = Library []

-- removeSong (addSong (addSong emptyLibrary unforgiven) one) one
