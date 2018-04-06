type rowT = {
  width: int,
  height: int,
  color: string
};

type svgRectT = {x: int, y: int, w: int, h: int, c: string};

type state = {
  text: string,
  error: option(string),
  output: option(array(svgRectT)),
  data: array(rowT),
};

/*width, height, color*/

type action =
  | UpdateText(string)
  | Compile
  | Reformat;

let component = ReasonReact.reducerComponent("Page");


let handleText = (event, self) => {
  let target = ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event));
  let content: string = target##value;
  self.ReasonReact.send(UpdateText(content))
};

type compiledJS;
[@bs.val] external parseRE : string => string = "";
[@bs.val] external printRE : string => string = "";
[@bs.val] external printML : string => string = "";
[@bs.scope "ocaml"] [@bs.val] external ocamlToJS : string => compiledJS = "compile";
[@bs.get] external getJSCode : compiledJS => string = "js_code";
[@bs.get] [@bs.return nullable] external getJSError : compiledJS => option(string) = "js_error_msg";
type processRowT = rowT => svgRectT;
[@bs.val] [@bs.return nullable] external evalJS : string => option(processRowT) = "eval";

let preamble = {|
type rowT = {width: int, height: int, color: string};
type svgRectT = {x: int, y: int, w: int, h: int, c: string};
let processRow = (row) => {
|};

let doCompile = (state) => {
  print_endline("Compiling...");
  let reason_code = state.text;
  let wrapped_code = preamble ++ reason_code ++ "}";
  let ocaml = printML(parseRE(wrapped_code));
  let js = ocamlToJS(ocaml);
  switch(getJSError(js)){
    | Some(error) => {...state, error: Some(error)}
    | None =>
        switch (evalJS(getJSCode(js))){
          | None => {...state, error: Some("Error when evaling code")}
          | Some(processRow) => {...state, output: Some(Array.map(processRow, state.data))}
        }
  }
};

let createRow = (id, row: rowT) : ReasonReact.reactElement => {
  <tr key=string_of_int(id)>
    <td style=ReactDOMRe.Style.make(~border="1px solid black", ())>(ReasonReact.stringToElement(string_of_int(row.width)))</td>
    <td style=ReactDOMRe.Style.make(~border="1px solid black", ())>(ReasonReact.stringToElement(string_of_int(row.height)))</td>
    <td style=ReactDOMRe.Style.make(~border="1px solid black", ())>(ReasonReact.stringToElement(row.color))</td>
  </tr>
};

let make = (_children) => {
  ...component,
  initialState: () => {
    text: "{x: 1, y: 1, w: row.width, h: row.height, c: row.color}",
    error: None,
    output: None,
    data: [|
      {width: 20, height: 40, color: "blue"},
      {width: 40, height: 30, color: "red"},
    |]},
  reducer: (action, state) =>
    switch (action) {
    | UpdateText(newText) => ReasonReact.Update({...state, text: newText})
    | Compile => {
      ReasonReact.Update(doCompile(state));
    }
    | Reformat => ReasonReact.Update({...state, text: printRE(parseRE(state.text))})
    },
  render: self =>
    <div>
      <table style=ReactDOMRe.Style.make(~border="1px solid black", ())>
      <thead>
        <tr>
          <th>(ReasonReact.stringToElement("width"))</th>
          <th>(ReasonReact.stringToElement("height"))</th>
          <th>(ReasonReact.stringToElement("color"))</th>
        </tr>
      </thead>
      <tbody>
      (ReasonReact.arrayToElement(Array.mapi(createRow, self.state.data)))
      </tbody>
      </table>
      <br/>
      <br/>
      (ReasonReact.stringToElement("(row) => {"))
      <br/>
      <textarea name="textarea" style=ReactDOMRe.Style.make(~marginLeft="30px", ~marginTop="5px", ~height="100px", ~width="300px", ()) value={self.state.text}
          onChange=(event => handleText(event, self)) ></textarea>
      <br/>
      (ReasonReact.stringToElement("}"))
      <br/>
      <br/>
      <button onClick=(_event => self.send(Compile))>
        (ReasonReact.stringToElement("Compile"))
      </button>
      <button onClick=(_event => self.send(Reformat))>
        (ReasonReact.stringToElement("Reformat"))
      </button>
      (switch (self.state.error) {
        | None => ReasonReact.nullElement
        | Some(error) => <div><br />(ReasonReact.stringToElement(error))</div>
      })
      <svg width="100" height="100">
        (switch (self.state.output) {
          | None => ReasonReact.nullElement
          | Some(output) => ReasonReact.arrayToElement(Array.map((svgRect) =>
          (<rect
              width=string_of_int(svgRect.w)
              height=string_of_int(svgRect.h)
              style=ReactDOMRe.Style.make(~fill=svgRect.c, ()) />), output))
        })
      </svg>
    </div>,
};
