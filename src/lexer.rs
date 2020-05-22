// Zonefile Format from RFC1035:
//
// The format of these files is a sequence of entries.  Entries are
// predominantly line-oriented, though parentheses can be used to continue
// a list of items across a line boundary, and text literals can contain
// CRLF within the text.  Any combination of tabs and spaces act as a
// delimiter between the separate items that make up an entry.  The end of
// any line in the master file can end with a comment.  The comment starts
// with a ";" (semicolon).
//
// The following entries are defined:
//
//     <blank>[<comment>]
//
//     $ORIGIN <domain-name> [<comment>]
//
//     $INCLUDE <file-name> [<domain-name>] [<comment>]
//
//     <domain-name><rr> [<comment>]
//
//     <blank><rr> [<comment>]
//
// The following entry was added by RFC2309 section 4.
//
//     $TTL <TTL> [<comment>]
//
// Blank lines, with or without comments, are allowed anywhere in the file.
//
// Two control entries are defined: $ORIGIN and $INCLUDE.  $ORIGIN is
// followed by a domain name, and resets the current origin for relative
// domain names to the stated name.  $INCLUDE inserts the named file into
// the current file, and may optionally specify a domain name that sets the
// relative domain name origin for the included file.  $INCLUDE may also
// have a comment.  Note that a $INCLUDE entry never changes the relative
// origin of the parent file, regardless of changes to the relative origin
// made within the included file.
//
// The last two forms represent RRs.  If an entry for an RR begins with a
// blank, then the RR is assumed to be owned by the last stated owner.  If
// an RR entry begins with a <domain-name>, then the owner name is reset.
//
// <rr> contents take one of the following forms:
//
//     [<TTL>] [<class>] <type> <RDATA>
//
//     [<class>] [<TTL>] <type> <RDATA>
//
// The RR begins with optional TTL and class fields, followed by a type and
// RDATA field appropriate to the type and class.  Class and type use the
// standard mnemonics, TTL is a decimal integer.  Omitted class and TTL
// values are default to the last explicitly stated values.  Since type and
// class mnemonics are disjoint, the parse is unique.  (Note that this
// order is different from the order used in examples and the order used in
// the actual RRs; the given order allows easier parsing and defaulting.)
//
// <domain-name>s make up a large share of the data in the master file.
// The labels in the domain name are expressed as character strings and
// separated by dots.  Quoting conventions allow arbitrary characters to be
// stored in domain names.  Domain names that end in a dot are called
// absolute, and are taken as complete.  Domain names which do not end in a
// dot are called relative; the actual domain name is the concatenation of
// the relative part with an origin specified in a $ORIGIN, $INCLUDE, or as
// an argument to the master file loading routine.  A relative name is an
// error when no origin is available.
//
// <character-string> is expressed in one or two ways: as a contiguous set
// of characters without interior spaces, or as a string beginning with a "
// and ending with a ".  Inside a " delimited string any character can
// occur, except for a " itself, which must be quoted using \ (back slash).
//
// Because these files are text files several special encodings are
// necessary to allow arbitrary data to be loaded.  In particular:
//
//                 of the root.
//
// @               A free standing @ is used to denote the current origin.
//
// \X              where X is any character other than a digit (0-9), is
//                 used to quote that character so that its special meaning
//                 does not apply.  For example, "\." can be used to place
//                 a dot character in a label.
//
// \DDD            where each D is a digit is the octet corresponding to
//                 the decimal number described by DDD.  The resulting
//                 octet is assumed to be text and is not checked for
//                 special meaning.
//
// ( )             Parentheses are used to group data that crosses a line
//                 boundary.  In effect, line terminations are not
//                 recognized within parentheses.
//
// ;               Semicolon is used to start a comment; the remainder of
//                 the line is ignored.

use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    zf: Peekable<Chars<'a>>,
    lineno: i32,
    charno: i32,
    state: State,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Origin {
        domain_name: String,
        lineno: i32,
    },
    Include {
        file_name: String,
        domain_name: Option<String>,
        lineno: i32,
    },
    TTL {
        ttl: i32,
        lineno: i32,
    },
    Text(String),
    DomainName(String),
    Comment,
    OpenParen,
    CloseParen,
}

#[derive(Clone, PartialEq, Debug)]
enum State {
    StartLine,
    RestOfLine, // Parse the reset of the line as normal
    Dollar,
    Origin,
    IncludeFileName,
    IncludeDomainName { file_name: String },
    Ttl,
    DomainName,
    Blank,
    Comment,
    WsOrComment, // The only valid thing left is whitespace/comments
    CommentLine, // The rest of the line is a comment
    Quote,
    EOL,
}

impl<'a> Lexer<'a> {
    pub fn new(zonefile: &str) -> Lexer {
        Lexer {
            zf: zonefile.chars().peekable(),
            lineno: 0,
            charno: 0,
            state: State::StartLine,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, &str> {
        let mut chars: Option<String> = None;

        loop {
            let ch = self.zf.peek();

            //println!(
            //    "ch = {:?}; state = {:?}(chars: {:?})",
            //    ch, self.state, chars
            //);

            match self.state {
                State::StartLine => match ch {
                    Some('\r') | Some('\n') => {
                        self.state = State::EOL;
                    }
                    Some(';') => {
                        self.state = State::Comment;
                        return Ok(Some(Token::Comment));
                    }
                    Some('$') => {
                        self.state = State::Dollar;
                        chars = Some(String::new());
                        self.next();
                    }
                    None => return Ok(None),
                    Some(_) => {
                        unimplemented!();
                    }
                },
                State::Dollar => match ch {
                    Some(ch) if ch.is_control() => {
                        return Err("Unexpected control character found");
                    }
                    Some(ch) if !ch.is_whitespace() => {
                        Self::push_to_str(&mut chars, *ch)?;
                        self.next();
                    }
                    Some(ch) if ch.is_whitespace() => {
                        let dollar: String = chars.take().unwrap();

                        if "INCLUDE" == dollar {
                            self.state = State::IncludeFileName;
                        } else if "ORIGIN" == dollar {
                            self.state = State::Origin;
                        } else if "TTL" == dollar {
                            self.state = State::Ttl;
                        } else {
                            return Err("Unknown control entry");
                        }

                        chars = Some(String::new());
                        self.next();
                    }
                    None | Some(_) => {
                        return Err("Unexpected end of line");
                    }
                },
                State::Origin => match ch {
                    Some(ch) if !ch.is_control() && !ch.is_whitespace() => {
                        Self::push_to_str(&mut chars, *ch)?;
                        self.next();
                    }
                    None | Some(_) => {
                        self.state = State::WsOrComment;
                        let domain_name = chars.take().unwrap_or_else(|| "".into());
                        return Ok(Some(Token::Origin {
                            domain_name: domain_name,
                            lineno: self.lineno,
                        }));
                    }
                },
                State::WsOrComment => match ch {
                    Some(';') => {
                        self.state = State::Comment;
                        return Ok(Some(Token::Comment));
                    }
                    Some(ch) if ch.is_whitespace() => {
                        self.next();
                    }
                    None | Some('\r') | Some('\n') => {
                        self.state = State::EOL;
                    }
                    Some(_) => {
                        return Err("Unexpected character found");
                    }
                },
                State::Comment => {
                    self.state = State::CommentLine;
                    chars = Some(String::new());
                    self.next();
                }
                State::CommentLine => match ch {
                    None | Some('\r') | Some('\n') => {
                        self.state = State::EOL;
                        return Ok(Some(Token::Text(chars.take().unwrap_or_else(|| "".into()))));
                    }
                    Some(ch) if ch.is_control() => {
                        return Err("Unexpected control character found");
                    }
                    Some(ch) => {
                        Self::push_to_str(&mut chars, *ch)?;
                        self.next();
                    }
                },
                State::EOL => match ch {
                    Some('\r') => {
                        self.next();
                    }
                    Some('\n') => {
                        self.lineno += 1;
                        self.charno = 0;
                        self.next();
                        self.state = State::StartLine;
                    }
                    Some(_) => {
                        return Err("Unexpected character found after carriage return");
                    }
                    None => {
                        return Ok(None);
                    }
                },
                _ => {
                    unimplemented!();
                }
            }
        }
    }

    fn next(&mut self) {
        self.zf.next();
        self.charno += 1;
    }

    fn push_to_str(chars: &'_ mut Option<String>, ch: char) -> Result<(), &'a str> {
        chars
            .as_mut()
            .ok_or_else(|| "chars is None".into())
            .and_then(|s| {
                s.push(ch);
                Ok(())
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn next_token(lexer: &mut Lexer) -> Option<Token> {
        let result = lexer.next_token();

        // Validate that the result is not an error
        assert_eq!(result.is_ok(), true);

        // Since we passed the above assert, unwrap
        return result.unwrap();
    }

    fn next_token_errors<'a>(lexer: &'a mut Lexer) -> Result<Option<Token>, &'a str> {
        let result = lexer.next_token();

        assert_eq!(result.is_err(), true);

        result
    }

    #[test]
    fn push_to_str_none() {
        let mut chars: Option<String> = None;

        assert_eq!(Lexer::push_to_str(&mut chars, 'a'), Err("chars is None"));
    }

    #[test]
    fn push_to_str() {
        let mut chars: Option<String> = Some(String::from("test"));

        assert_eq!(Lexer::push_to_str(&mut chars, 'i'), Ok(()));
        assert_eq!(Lexer::push_to_str(&mut chars, 'n'), Ok(()));
        assert_eq!(Lexer::push_to_str(&mut chars, 'g'), Ok(()));
        assert_eq!(chars.unwrap(), "testing");
    }

    #[test]
    fn comment_only() {
        let zonefile = "; this is a comment\n";
        let mut lexer = Lexer::new(zonefile);
        assert_eq!(next_token(&mut lexer), Some(Token::Comment));
        assert_eq!(
            next_token(&mut lexer),
            Some(Token::Text(" this is a comment".into()))
        );
        assert_eq!(next_token(&mut lexer), None);
    }

    #[test]
    fn multiple_comment() {
        let zonefile = "; this is a comment\n; this is another comment";
        let mut lexer = Lexer::new(zonefile);
        assert_eq!(next_token(&mut lexer), Some(Token::Comment));
        assert_eq!(
            next_token(&mut lexer),
            Some(Token::Text(" this is a comment".into()))
        );
        assert_eq!(next_token(&mut lexer), Some(Token::Comment));
        assert_eq!(
            next_token(&mut lexer),
            Some(Token::Text(" this is another comment".into()))
        );
        assert_eq!(next_token(&mut lexer), None);
    }

    #[test]
    fn carriagereturn_newlines() {
        assert_eq!(next_token(&mut Lexer::new("\r\n\r\n")), None);
    }

    #[test]
    fn newlines() {
        assert_eq!(next_token(&mut Lexer::new("\n\n")), None);
    }

    #[test]
    fn carriagereturn_no_nl() {
        assert_eq!(
            next_token_errors(&mut Lexer::new("\rtest")),
            Err("Unexpected character found after carriage return")
        );
    }

    #[test]
    fn origin_only() {
        let mut lexer = Lexer::new("$ORIGIN cidr.network.");
        assert_eq!(
            next_token(&mut lexer),
            Some(Token::Origin {
                domain_name: "cidr.network.".into(),
                lineno: 0
            })
        );
        assert_eq!(next_token(&mut lexer), None);
    }

    #[test]
    fn origin_with_comment() {
        let mut lexer = Lexer::new("$ORIGIN cidr.network. ; this is a comment");
        assert_eq!(
            next_token(&mut lexer),
            Some(Token::Origin {
                domain_name: "cidr.network.".into(),
                lineno: 0
            })
        );
        assert_eq!(next_token(&mut lexer), Some(Token::Comment));
        assert_eq!(
            next_token(&mut lexer),
            Some(Token::Text(" this is a comment".into()))
        );
        assert_eq!(next_token(&mut lexer), None);
    }

    #[test]
    fn origin_invalid_data() {
        let zonefile = "$ORIGIN cidr.network. stray characters";
        let mut lexer = Lexer::new(zonefile);
        assert_eq!(
            next_token(&mut lexer),
            Some(Token::Origin {
                domain_name: "cidr.network.".into(),
                lineno: 0
            })
        );
        assert_eq!(
            next_token_errors(&mut lexer),
            Err("Unexpected character found")
        );
    }
}
