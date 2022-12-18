use std::{borrow::Cow, collections::BTreeMap};

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

impl BuildElement for Vec<Node> {
    fn with_child<T: Into<Node>>(mut self, child: T) -> Self {
        self.push(child.into());
        self
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
