use xml_macro::xml;

fn main() {
    let some_value = 3;
    let the_answer = 42;
    let tree = "tree";
    let doc = xml!(
        <html>
        <head />
        <body type="emily" ns:value={some_value}>
        <p>"lorem ipsum dolor sit amet"</p>
        {the_answer}"bar"
        #{xml_gen::Element::new("a")}
        #tree
        </body>
        </html>
    );
    println!("{doc:#4}");
    println!("{doc}");
}
