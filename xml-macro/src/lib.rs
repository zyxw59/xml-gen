use std::{collections::BTreeSet, iter::Peekable};

use proc_macro2::{Delimiter, Ident, Punct, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned};

#[proc_macro]
pub fn debug_tokens(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let s = input
        .into_iter()
        .map(|tt| format!("{tt:?}\n"))
        .collect::<String>();
    quote!(#s).into()
}

#[proc_macro]
pub fn xml(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Parser::parse(TokenStream::from(input))
        .unwrap_or_else(TokenStream::from)
        .into()
}

struct Parser<T>
where
    T: Iterator,
{
    tag_stack: Vec<String>,
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = TokenTree>,
{
    pub fn parse<I>(tokens: I) -> Result<TokenStream, Error>
    where
        I: IntoIterator<IntoIter = T>,
    {
        let mut parser = Parser {
            tag_stack: Vec::new(),
            tokens: tokens.into_iter().peekable(),
        };
        let xml_gen = quote::format_ident!("xml_gen");
        let mut output = quote!(::std::vec::Vec::<::#xml_gen::Node>::new());
        while let Some(tokens) = parser.parse_next()? {
            output.extend(tokens);
        }
        if !parser.tag_stack.is_empty() {
            Err(Error::Token {
                actual: None,
                expected: "`<`, `{`, `#`, or string literal",
            })
        } else {
            Ok(quote!({
                use ::#xml_gen::BuildElement;
                #output
            }))
        }
    }

    fn parse_next(&mut self) -> Result<Option<TokenStream>, Error> {
        match self.tokens.next() {
            None => Ok(None),
            Some(TokenTree::Punct(p)) if p.as_char() == '<' => self.parse_tag(),
            Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
                let (expr, span) = self.expect_expr()?;
                Ok(Some(quote_spanned!(span=> .with_child(#expr))))
            }
            Some(TokenTree::Literal(lit)) => {
                Ok(Some(quote_spanned!(lit.span()=> .with_child(#lit))))
            }
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                Ok(Some(quote_spanned!(group.span()=> .with_child(#group.to_string()))))
            }
            actual => {
                return Err(Error::Token {
                    actual,
                    expected: "`<`, `{`, `#`, or string literal",
                })
            }
        }
    }

    fn parse_tag(&mut self) -> Result<Option<TokenStream>, Error> {
        match self.tokens.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == '/' => {
                let id = self.expect_ident()?;
                let tag = match self.tokens.next() {
                    Some(TokenTree::Punct(p)) if p.as_char() == '>' => id.to_string(),
                    Some(TokenTree::Punct(p)) if p.as_char() == ':' => {
                        let id2 = self.expect_ident()?;
                        self.expect_punct(PunctChar::RightAngle)?;
                        format!("{id}:{id2}")
                    }
                    actual => {
                        return Err(Error::Token {
                            actual,
                            expected: "`>` or `:`",
                        })
                    }
                };
                let expected = self.tag_stack.pop();
                if Some(tag) != expected {
                    Err(Error::CloseTag {
                        expected,
                        actual: id,
                    })
                } else {
                    Ok(Some(quote_spanned!(id.span()=> .finish())))
                }
            }
            Some(TokenTree::Ident(id)) => {
                let (tag, tag_span) = match self.tokens.peek() {
                    Some(TokenTree::Punct(p)) if p.as_char() == ':' => {
                        self.expect_punct(PunctChar::Colon)?;
                        let id2 = self.expect_ident()?;
                        (format!("{id}:{id2}"), id2.span())
                    }
                    _ => (id.to_string(), id.span()),
                };
                let mut output = quote_spanned!(tag_span=> .build_child(#tag));
                self.tag_stack.push(tag);
                // handle attributes
                let mut seen_attrs = BTreeSet::new();
                loop {
                    match self.tokens.next() {
                        Some(TokenTree::Punct(p)) if p.as_char() == '>' => {
                            break Ok(Some(output));
                        }
                        Some(TokenTree::Punct(p)) if p.as_char() == '/' => {
                            self.expect_punct(PunctChar::RightAngle)?;
                            self.tag_stack.pop();
                            output.extend(quote_spanned!(p.span()=> .finish_self_closing()));
                            break Ok(Some(output));
                        }
                        Some(TokenTree::Ident(id)) => {
                            output.extend(self.parse_attribute(id, &mut seen_attrs)?);
                        }
                        actual => {
                            break Err(Error::Token {
                                expected: "`>`, `/`, or ident",
                                actual,
                            })
                        }
                    }
                }
            }
            actual => Err(Error::Token {
                actual,
                expected: "ident or `/`",
            }),
        }
    }

    fn parse_attribute(
        &mut self,
        initial: Ident,
        seen_attrs: &mut BTreeSet<String>,
    ) -> Result<TokenStream, Error> {
        let (attr_name, name_span) = match self.tokens.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == '=' => {
                (initial.to_string(), initial.span())
            }
            Some(TokenTree::Punct(p)) if p.as_char() == ':' => {
                let id2 = self.expect_ident()?;
                self.expect_punct(PunctChar::Equal)?;
                (format!("{initial}:{id2}"), id2.span())
            }
            actual => {
                return Err(Error::Token {
                    expected: "`=` or `:`",
                    actual,
                })
            }
        };
        if !seen_attrs.insert(attr_name.clone()) {
            return Err(Error::DuplicateAttribute {
                attribute: attr_name,
                span: name_span,
            });
        }
        let attr_body = match self.tokens.next() {
            Some(TokenTree::Literal(lit)) => quote_spanned!(lit.span()=> #lit),
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                quote_spanned!(group.span()=> #group.to_string())
            }
            Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
                let (expr, _span) = self.expect_expr()?;
                expr
            }
            actual => {
                return Err(Error::Token {
                    expected: "string literal or `{`",
                    actual,
                })
            }
        };
        Ok(quote_spanned!(name_span=> .with_attribute(#attr_name, #attr_body)))
    }

    fn expect_ident(&mut self) -> Result<Ident, Error> {
        match self.tokens.next() {
            Some(TokenTree::Ident(id)) => Ok(id),
            actual => Err(Error::Token {
                expected: "ident",
                actual,
            }),
        }
    }

    fn expect_punct(&mut self, ch: PunctChar) -> Result<Punct, Error> {
        match self.tokens.next() {
            Some(TokenTree::Punct(p)) if p.as_char() == ch.as_char() => Ok(p),
            actual => Err(Error::Token {
                expected: ch.as_str(),
                actual,
            }),
        }
    }

    fn expect_expr(&mut self) -> Result<(TokenStream, Span), Error> {
        match self.tokens.next() {
            Some(TokenTree::Ident(id)) => Ok((quote_spanned!(id.span()=> #id), id.span())),
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                let stream = group.stream();
                Ok((quote_spanned!(group.span()=> #stream), group.span()))
            }
            actual => Err(Error::Token {
                expected: "`{` or ident",
                actual,
            }),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum PunctChar {
    RightAngle,
    Equal,
    Colon,
}

impl PunctChar {
    fn as_char(self) -> char {
        match self {
            PunctChar::RightAngle => '>',
            PunctChar::Equal => '=',
            PunctChar::Colon => ':',
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            PunctChar::RightAngle => "`>`",
            PunctChar::Equal => "`=`",
            PunctChar::Colon => "`:`",
        }
    }
}

#[derive(Clone, Debug)]
enum Error {
    CloseTag {
        expected: Option<String>,
        actual: Ident,
    },
    Token {
        expected: &'static str,
        actual: Option<TokenTree>,
    },
    DuplicateAttribute {
        attribute: String,
        span: Span,
    },
}

impl From<Error> for TokenStream {
    fn from(err: Error) -> TokenStream {
        match err {
            Error::CloseTag {
                expected: Some(expected),
                actual,
            } => {
                let msg = format!("Unexpected close tag `</{actual}>`, expected `</{expected}>`");
                quote_spanned!(actual.span()=> compile_error!(#msg))
            }
            Error::CloseTag {
                expected: None,
                actual,
            } => {
                let msg =
                    format!("Unexpected close tag with no corresponing open tag: `</{actual}>`");
                quote_spanned!(actual.span()=> compile_error!(#msg))
            }
            Error::Token {
                expected,
                actual: Some(actual),
            } => {
                let msg = format!("Unexpected token `{actual}`, expected {expected}");
                quote_spanned!(actual.span()=> compile_error!(#msg))
            }
            Error::Token {
                expected,
                actual: None,
            } => {
                let msg = format!("Unexpected end of macro input, expected {expected}");
                quote!(compile_error!(#msg))
            }
            Error::DuplicateAttribute { attribute, span } => {
                let msg = format!("Duplicate attribute `{attribute}`");
                quote_spanned!(span=> compile_error!(#msg))
            }
        }
    }
}
