private package Kit.SQL.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier,
       Tok_Integer_Constant, Tok_Float_Constant,
       Tok_String_Constant, Tok_Character_Constant,

       Tok_And, Tok_As, Tok_By, Tok_From, Tok_Group,
       Tok_Having, Tok_Join, Tok_Not, Tok_Or, Tok_Order,
       Tok_Select, Tok_Where,

       Tok_Comma, Tok_Semicolon, Tok_Dot, Tok_Asterisk,
       Tok_Left_Paren, Tok_Right_Paren,
       Tok_EQ, Tok_NE, Tok_GT, Tok_LT, Tok_GE, Tok_LE);

end Kit.SQL.Tokens;
