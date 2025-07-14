import CodeMirrorEditor from "./CodeMirrorEditor";
import MonacoEditor from "./MonacoEditor";
import { ide } from "../IDE";

export default function CodeEditorBackend(props) {
	const backend = ide.settings.section("editor").get("backend");
	const EditorComponent =
		backend === "Monaco" ? MonacoEditor : CodeMirrorEditor;
	return <EditorComponent {...props} />;
}
