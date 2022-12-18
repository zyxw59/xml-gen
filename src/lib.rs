use std::{borrow::Cow, collections::BTreeMap, fmt};

#[derive(Clone, Debug)]
pub struct Element {
    pub tag: &'static str,
    pub attributes: BTreeMap<&'static str, Cow<'static, str>>,
    pub children: Vec<Node>,
    /// Whether the element can use the self-closing tag form (`<tag />`).
    pub can_be_self_closing: bool,
}

impl Element {
    pub fn new(tag: &'static str) -> Self {
        Element {
            tag,
            attributes: Default::default(),
            children: Default::default(),
            can_be_self_closing: false,
        }
    }

    fn formatter(&self, level: usize) -> ElementFormatter {
        ElementFormatter { el: self, level }
    }
}

impl fmt::Display for Element {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.formatter(0), f)
    }
}

struct ElementFormatter<'a> {
    el: &'a Element,
    level: usize,
}

const DEFAULT_INDENT: usize = 2;

impl fmt::Display for ElementFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let width = f.width().unwrap_or(DEFAULT_INDENT) * self.level;
        if f.alternate() && self.level > 0 {
            write!(f, "\n{:width$}<{}", " ", self.el.tag)?;
        } else {
            write!(f, "<{}", self.el.tag)?;
        }
        for (attr, value) in &self.el.attributes {
            write!(f, r#" {attr}="{value}""#)?;
        }
        if self.el.can_be_self_closing && self.el.children.is_empty() {
            f.write_str(" />")
        } else {
            f.write_str(">")?;
            if self.el.children.is_empty() {
                write!(f, "</{}>", self.el.tag)
            } else if f.alternate() {
                for child in &self.el.children {
                    fmt::Display::fmt(&child.formatter(self.level + 1), f)?;
                }
                write!(f, "\n{:width$}</{}>", " ", self.el.tag)
            } else {
                let mut was_text_node = false;
                for child in &self.el.children {
                    if child.is_text() {
                        if was_text_node {
                            f.write_str(" ")?;
                        }
                        was_text_node = true;
                    } else {
                        was_text_node = false;
                    }
                    fmt::Display::fmt(child, f)?;
                }
                write!(f, "</{}>", self.el.tag)
            }
        }
    }
}

pub trait BuildElement {
    fn with_child<T: Into<Node>>(self, child: T) -> Self;

    fn build_child(self, tag: &'static str) -> ElementBuilder<Self>
    where
        Self: Sized,
    {
        ElementBuilder::new(self, tag)
    }
}

impl BuildElement for Element {
    fn with_child<T: Into<Node>>(mut self, child: T) -> Self {
        self.children.push(child.into());
        self
    }
}

#[derive(Clone, Debug, Default)]
pub struct Document {
    pub root: Option<Element>,
}

impl Document {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn build_child(self, tag: &'static str) -> ElementBuilder<Self> {
        ElementBuilder {
            parent: self,
            finish: Self::with_root,
            child: Element::new(tag),
        }
    }

    fn with_root(mut self, root: Element) -> Self {
        self.root = Some(root);
        self
    }
}

impl fmt::Display for Document {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(root) = &self.root {
            fmt::Display::fmt(root, f)
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug)]
#[must_use = "ElementBuilder does nothing if its `finish` or `finish_self_closing` method is not called"]
pub struct ElementBuilder<T> {
    parent: T,
    finish: fn(T, Element) -> T,
    child: Element,
}

impl<T> ElementBuilder<T>
where
    T: BuildElement,
{
    pub fn new(parent: T, tag: &'static str) -> Self {
        ElementBuilder {
            parent,
            finish: <T as BuildElement>::with_child::<Element>,
            child: Element::new(tag),
        }
    }

    pub fn with_attribute<S>(mut self, name: &'static str, value: S) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        self.child.attributes.insert(name, value.into());
        self
    }
}

impl<T> ElementBuilder<T> {
    pub fn finish(self) -> T {
        (self.finish)(self.parent, self.child)
    }

    pub fn finish_self_closing(mut self) -> T {
        self.child.can_be_self_closing = true;
        self.finish()
    }
}

impl<P> BuildElement for ElementBuilder<P> {
    fn with_child<T: Into<Node>>(mut self, child: T) -> Self {
        self.child = self.child.with_child(child);
        self
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    Element(Element),
    Text(Cow<'static, str>),
}

impl Node {
    fn formatter(&self, level: usize) -> NodeFormatter {
        NodeFormatter { node: self, level }
    }

    fn is_text(&self) -> bool {
        matches!(self, Node::Text(_))
    }
}

impl From<Element> for Node {
    fn from(el: Element) -> Node {
        Node::Element(el)
    }
}

impl<T> From<T> for Node
where
    T: Into<Cow<'static, str>>,
{
    fn from(val: T) -> Node {
        Node::Text(val.into())
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.formatter(0), f)
    }
}

struct NodeFormatter<'a> {
    node: &'a Node,
    level: usize,
}

impl fmt::Display for NodeFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let width = f.width().unwrap_or(DEFAULT_INDENT) * self.level;
        match self.node {
            Node::Element(el) => fmt::Display::fmt(&el.formatter(self.level), f),
            Node::Text(text) => {
                if f.alternate() {
                    write!(f, "\n{:width$}{text}", "")
                } else {
                    f.write_str(text)
                }
            }
        }
    }
}
