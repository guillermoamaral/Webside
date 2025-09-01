import { forwardRef } from "react";
import CodeMirrorEditor from "./CodeMirrorEditor";
import MonacoEditor from "./MonacoEditor";
import { ide } from "../IDE";

const CodeEditorBackend = forwardRef((props, ref) => {
	const backend = ide.settings.section("editor").get("backend");
	const EditorComponent =
		backend === "Monaco" ? MonacoEditor : CodeMirrorEditor;
	return <EditorComponent ref={ref} {...props} />;
});

export default CodeEditorBackend;
