type resultT('a, 'b) = Ok('a) | Error('b);

type rowT = {
  width: int,
  height: int,
  color: string
};

type svgRectT = {x: int, y: int, w: int, h: int, c: string};

type fnT = {name: string, body: string};

type state = {
  topLevel: string,
  fnList: list(fnT),
  error: option(string),
  output: option(array(svgRectT)),
  data: array(rowT),
};

type action =
  | UpdateAndCompile(int, string)
  | UpdateTopLevelAndCompile(string)
  | DeleteTextBox(int);

let filteri = (f, lst) => {
  let (_, lst) = List.fold_left(((i, l), v) => {
        if (f(i, v)) {
          (i + 1, [v, ...l])
        } else {
          (i + 1, l)
        }
      }, (0, []), lst);
  lst
};

let component = ReasonReact.reducerComponent("Page");

let handleTopLevel = (event, self) => {
  let target = ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event));
  let content: string = target##value;
  self.ReasonReact.send(UpdateTopLevelAndCompile(content))
};

let handleText = (codeNum, event, self) => {
  let target = ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event));
  let content: string = target##value;
  if (content == "") {
    self.ReasonReact.send(DeleteTextBox(codeNum))
  } else {
    self.ReasonReact.send(UpdateAndCompile(codeNum, content))
  }
};

type ocamlAST;
type compiledJS;
[@bs.val] external parseRE : string => ocamlAST = "";
[@bs.val] external printRE : ocamlAST => string = "";
[@bs.val] external printML : ocamlAST => string = "";
[@bs.scope "ocaml"] [@bs.val] external ocamlToJS : string => compiledJS = "compile";
[@bs.get] external getJSCode : compiledJS => string = "js_code";
[@bs.get] [@bs.return nullable] external getJSError : compiledJS => option(string) = "js_error_msg";
/* type processRowT = rowT => svgRectT; */
type processRowT = array(rowT) => array(svgRectT);
[@bs.val] [@bs.return nullable] external evalJS : string => option(processRowT) = "eval";

let preamble = {|
type rowT = {width: int, height: int, color: string};
type svgRectT = {x: int, y: int, w: int, h: int, c: string};
|};
/* let processRow = (row: rowT) : svgRectT => { row */


let wrapFunction = (fn) => Printf.sprintf("let %s = (x) => {%s};", fn.name, fn.body);

let suffix = (topLevel) => "\nlet processTable = (table: array(rowT)) : array(svgRectT) => {" ++ topLevel ++ "}";

let doCompile = (state) => {
  print_endline("Compiling...");
  let reason_code = String.concat("\n", List.map(wrapFunction, state.fnList));
  let wrapped_code = preamble ++ reason_code ++ suffix(state.topLevel);
  print_endline(wrapped_code);
  let maybeRE = try (Ok(parseRE(wrapped_code))) {
    | Js.Exn.Error(e) =>
      switch (Js.Exn.message(e)) {
        | Some(message) => Error(message)
        | None => Error("Unknown reason parsing error")
        }
  };
  switch (maybeRE) {
    | Ok(ocamlAST) =>
      let ocaml = printML(ocamlAST);
      /* print_endline(ocaml); */
      let js = ocamlToJS(ocaml);
      switch(getJSError(js)){
        | Some(error) =>
        Js.Re.fromString("Unbound value ([a-zA-Z][a-zA-Z1-9_]*)")
          |> Js.Re.exec(error)
          |> (
            fun
            | Some(result) => switch(Js.Nullable.to_opt(Js.Re.captures(result)[1])){
              | Some(fnName) => {...state, fnList: [{name:fnName, body: ""}, ...state.fnList]}
              /* print_endline("NAME::::" ++ fnName); state */
              | None => {...state, error: Some(error)}
              }
            | None => {...state, error: Some(error)}
          );
        | None =>
            /* print_endline(getJSCode(js)); */
            switch (evalJS(getJSCode(js))){
              | None => {...state, error: Some("Error when evaling code")}
              | Some(processRow) => Js.log(processRow);
              {...state, output: Some(processRow(state.data)), error: None}
            }
      }

    | Error(msg) => {...state, error: Some(msg)}
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
    topLevel: "Array.map(processRow, table)",
    fnList: [{name: "processRow", body: "{x: 1, y: 1, w: x.width, h: x.height, c: x.color}"}],
    error: None,
    output: None,
    data: [|
      {width: 20, height: 40, color: "blue"},
      {width: 40, height: 30, color: "red"},
    |]},
  reducer: (action, state) =>
    switch (action) {
    | UpdateAndCompile(textNum, text : string) =>
        let newFnList = List.mapi((i, fn) => if (i === textNum) {...fn, body: text} else fn, state.fnList);
        ReasonReact.Update(doCompile({...state, fnList: newFnList}));
    | UpdateTopLevelAndCompile(text : string) =>
        ReasonReact.Update(doCompile({...state, topLevel: text}));
    | DeleteTextBox(textNum) =>
        let newFnList = filteri((i, x) => i !== textNum, state.fnList);
        ReasonReact.Update(doCompile({...state, fnList: newFnList}));
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
        (ReasonReact.stringToElement("(table) => {"))
        <br/>
        <textarea name="textarea"
            style=ReactDOMRe.Style.make(~marginLeft="30px", ~marginTop="5px", ~height="100px", ~width="300px", ())
            value={self.state.topLevel}
            onChange=(event => handleTopLevel(event, self)) ></textarea>
        <br/>
        (ReasonReact.stringToElement("}"))
        <br/>
      <br/>
      (ReasonReact.arrayToElement(Array.mapi((i, fn) => (
        <div key=string_of_int(i)>
          (ReasonReact.stringToElement("let " ++ fn.name ++ " = (x) => {"))
          <br/>
          <textarea name="textarea"
              style=ReactDOMRe.Style.make(~marginLeft="30px", ~marginTop="5px", ~height="100px", ~width="300px", ())
              value={fn.body}
              onChange=(event => handleText(i, event, self)) ></textarea>
          <br/>
          (ReasonReact.stringToElement("}"))
          <br/>
        </div>
        ), Array.of_list(self.state.fnList))))
      <br/>
      (switch (self.state.error) {
        | None => ReasonReact.nullElement
        | Some(error) => <div><br />(ReasonReact.stringToElement(error))</div>
      })
      <svg width="100" height="100">
        (switch (self.state.output) {
          | None => ReasonReact.nullElement
          | Some(output) => ReasonReact.arrayToElement(Array.mapi((i, svgRect) =>
          (<rect
              key=string_of_int(i)
              width=string_of_int(svgRect.w)
              height=string_of_int(svgRect.h)
              style=ReactDOMRe.Style.make(~fill=svgRect.c, ()) />), output))
        })
      </svg>
    </div>,
};
