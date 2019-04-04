with GCS.Lexer;
with GCS.Styles;                       use GCS.Styles;

with Kit.SQL.Tokens;                   use Kit.SQL.Tokens;

pragma Elaborate_All (GCS.Lexer);

private package Kit.SQL.Lexical is
  new GCS.Lexer (Token              => Token,
                 Tok_None           => Tok_None,
                 Tok_End_Of_File    => Tok_End_Of_File,
                 Tok_Bad_Character  => Tok_Bad_Character,
                 Tok_Identifier     => Tok_Identifier,
                 Tok_String         => Tok_String_Constant,
                 Tok_Character      => Tok_Character_Constant,
                 Tok_Integer        => Tok_Integer_Constant,
                 Tok_Float          => Tok_Float_Constant,
                 First_Keyword      => Tok_And,
                 Keywords           => "and as by exit from group having "
                                     & "join not or order select where",
                 First_Symbol       => Tok_Comma,
                 Symbols            => ", ; . * ( ) = /= > < >= <=",
                 Identifier_Start   => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "_",
                 Identifier_Body    => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "0123456789" &
                                       "_",
                 Line_Comment_Start => "--",
                 Properties         => (Multi_Characters => True,
                                        others => False));
