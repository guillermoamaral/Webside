import CodeMirrorDiffEditor from "./CodeMirrorDiffEditor";
import MonacoDiffEditor from "./MonacoDiffEditor";
import { ide } from "../IDE";

export default function CodeDiffEditorBackend(props) {
	const backend = ide.settings.section("editor").get("backend");
	const DiffEditorComponent =
		backend === "Monaco" ? MonacoDiffEditor : CodeMirrorDiffEditor;
	return <DiffEditorComponent {...props} />;
}
