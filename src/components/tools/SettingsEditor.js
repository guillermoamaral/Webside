import Tool from "./Tool";
import {
    Typography,
    Box,
    Button,
    Link,
    Divider,
    Paper,
    TextField,
} from "@mui/material";
import SettingEditor from "../parts/SettingEditor";
import CustomTree from "../controls/CustomTree";
import CustomSplit from "../controls/CustomSplit";
import CustomPaper from "../controls/CustomPaper";
import BackendTester from "./BackendTester";
import { ide } from "../IDE";
import Scrollable from "../controls/Scrollable";
import CodeEditorBackend from "../parts/CodeEditorBackend";
import React from "react";
import CustomList from "../controls/CustomList";

class SettingsEditor extends Tool {
    constructor(props) {
        super(props);
        let section = null;
        let expandedSections = [];
        if (props.path) {
            const setting = props.settings.setting(props.path);
            section = setting
                ? setting.section()
                : props.settings.section(props.path);
            expandedSections = section ? section.sectionPath() : [];
        }
        this.state = {
            selectedSection: section,
            expandedSections: expandedSections,
            error: null,
            searchText: "",
            selectedSettingPath: props.path,
            highlightedSetting: props.path,
        };
        this.searchInputRef = React.createRef();
        this.scrollableRef = React.createRef();
        this.settingRefs = {};
        this.highlightTimer = null;
        
        if (props.path) {
            setTimeout(() => {
                this.scrollToSetting(props.path);
                if (this.highlightTimer) {
                    clearTimeout(this.highlightTimer);
                }
                this.highlightTimer = setTimeout(() => {
                    this.setState({ highlightedSetting: null });
                }, 1000);
            }, 100);
        }
    }

    componentDidUpdate(prevProps, prevState) {
        if (
            prevState.highlightedSetting !== this.state.highlightedSetting &&
            this.state.highlightedSetting
        ) {
            if (this.highlightTimer) {
                clearTimeout(this.highlightTimer);
            }
            this.highlightTimer = setTimeout(() => {
                this.setState({ highlightedSetting: null });
            }, 1000);
        }
    }

    componentWillUnmount() {
        if (this.highlightTimer) {
            clearTimeout(this.highlightTimer);
        }
    }

    apply = () => {
        const handler = this.props.onApply;
        if (handler) handler(this.props.settings);
    };

    resetSelectedSection = (event) => {
        event.preventDefault();
        const section = this.state.selectedSection;
        const handler = this.props.onResetSection;
        if (handler) handler(section.path());
        this.forceUpdate();
    };

    sectionSelected = async (section) => {
        this.setState({ selectedSection: section });
    };

    sectionExpanded = async (section) => {
        this.setState({
            expandedSections: [...this.state.expandedSections, section],
        });
    };

    sectionCollapsed = (section) => {
        const expanded = this.state.expandedSections;
        expanded.splice(expanded.indexOf(section), 1);
        this.setState({ expandedSections: expanded });
    };

    sampleMethod = () => {
        return `method: argument
	"This is a comment"
	| temporaryVariable |
	temporaryVariable := 123 + self unary.
	self keyword1: true keyword2: #symbol keyword3: nil.
	instanceVariable := 'string'.
	self messageWithLintError; messageWithLintInfo.
	super messageWithLintWarning.
	^SomeClass new`;
    };

    sampleAnnotations = () => {
        return [
            {
                from: 193,
                to: 213,
                type: "error",
                description: "This is a sample error",
            },
            {
                from: 215,
                to: 234,
                type: "warning",
                description: "This is a sample warning",
            },
            {
                from: 243,
                to: 265,
                type: "info",
                description: "This is a sample info",
            },
        ];
    };

    settingChanged = (setting) => {
        if (setting.name === "theme") ide.applyTheme(this.props.settings);
        if (["dark", "light"].some((s) => setting.path().includes(s))) {
            this.props.settings.set("appearance.theme", "Custom");
        }
        if (setting.path().includes("appearance")) {
            if (this.codePreviewRef) {
                this.codePreviewRef.forceUpdate();
            }
        }
    };

    searchTextChanged = (text) => {
        this.setState({ searchText: text });
    };

    selectPath = (path) => {
        const settings = this.props.settings;
        const setting = settings.setting(path);
        const section = setting ? setting.section() : settings.section(path);
        const expanded = this.state.expandedSections;
        if (section) {
            section.sectionPath().forEach((s) => {
                if (!expanded.includes(s)) expanded.push(s);
            });
        }
        console.log(path);
        if (setting) {
            this.setState({
                selectedSection: section,
                expandedSections: expanded,
                searchText: "",
                selectedSettingPath: path,
                highlightedSetting: path,
            });
            setTimeout(() => this.scrollToSetting(path), 100);
        } else {
            this.setState({
                selectedSection: section,
                expandedSections: expanded,
                searchText: "",
                selectedSettingPath: null,
                highlightedSetting: null,
            });
        }
    };

    scrollToSetting = (settingPath) => {
        if (this.scrollableRef.current && this.settingRefs[settingPath]) {
            this.settingRefs[settingPath].scrollIntoView({
                behavior: "smooth",
                block: "start",
            });
        }
    };

    render() {
        const settings = this.props.settings;
        const {
            selectedSection,
            expandedSections,
            searchText,
            highlightedSetting,
        } = this.state;
        const selectedSettings = selectedSection
            ? selectedSection.plainSettings()
            : [];
        const maxLabelLength = selectedSettings.reduce(
            (max, s) => Math.max(max, s.label.length),
            0
        );
        return (
            <Box
                display="flex"
                flexDirection="column"
                sx={{ width: "100%", height: "100%" }}
            >
                <CustomSplit>
                    <Box sx={{ width: "15%" }}>
                        <Box
                            display="flex"
                            flexDirection="column"
                            sx={{ height: "100%" }}
                        >
                            <Box>
                                <TextField
                                    inputRef={this.searchInputRef}
                                    value={searchText}
                                    onChange={(event) =>
                                        this.searchTextChanged(
                                            event.target.value
                                        )
                                    }
                                    size="small"
                                    placeholder="Search settings..."
                                    name="searchText"
                                    fullWidth
                                    margin="dense"
                                    autoFocus
                                    type="text"
                                />
                            </Box>
                            <Box flexGrow={1}>
                                <CustomPaper>
                                    {searchText.length > 0 && (
                                        <Box flexGrow={1} height="100%">
                                            <CustomList
                                                itemColor={(s) => null}
                                                itemLabel={(s) => s.labelPath()}
                                                items={settings.findSettings(
                                                    searchText
                                                )}
                                                onItemSelect={(s) =>
                                                    this.selectPath(s.path())
                                                }
                                            />
                                        </Box>
                                    )}
                                    {searchText.length === 0 && (
                                        <CustomTree
                                            nodes={settings.sections()}
                                            nodeLabel="label"
                                            nodeChildren={(s) => s.sections()}
                                            selectedNode={selectedSection}
                                            onNodeSelect={this.sectionSelected}
                                            expandedNodes={expandedSections}
                                            onNodeExpand={this.sectionExpanded}
                                            onNodeCollapse={
                                                this.sectionCollapsed
                                            }
                                        />
                                    )}
                                </CustomPaper>
                            </Box>
                        </Box>
                    </Box>
                    {selectedSection && (
                        <Box
                            display="flex"
                            flexDirection="column"
                            ml={2}
                            flexGrow={1}
                            sx={{ width: "85%" }}
                        >
                            <Typography variant="h6">
                                {selectedSection.label}
                            </Typography>
                            <Divider />
                            <Box
                                mt={1}
                                display="flex"
                                direction="row"
                                justifyContent="flex-end"
                                alignItems="center"
                            >
                                <Link
                                    variant="caption"
                                    onClick={this.resetSelectedSection}
                                    color="inherit"
                                    underline="hover"
                                    href="#"
                                >
                                    {"Reset " +
                                        selectedSection.label +
                                        " settings"}
                                </Link>
                            </Box>
                            <Box
                                m={3}
                                flexGrow={1}
                                display="flex"
                                flexDirection="row"
                            >
                                <Box
                                    display="flex"
                                    flexDirection="column"
                                    sx={{
                                        width: "100%",
                                        height: "100%",
                                        marginLeft: 1,
                                        marginRight: 2,
                                    }}
                                >
                                    <Scrollable ref={this.scrollableRef}>
                                        {selectedSettings.map((setting) => (
                                            <Box
                                                key={setting.path()}
                                                mb={1}
                                                mr={2}
                                                ref={(el) => {
                                                    if (el)
                                                        this.settingRefs[
                                                            setting.path()
                                                        ] = el;
                                                }}
                                            >
                                                <Paper
                                                    elevation={0}
                                                    sx={{
                                                        p: 1,
                                                        backgroundColor:
                                                            highlightedSetting ===
                                                            setting.path()
                                                                ? "primary.light"
                                                                : "action.hover",
                                                        borderRadius: 2,
                                                        transition:
                                                            "background-color 0.3s ease",
                                                    }}
                                                >
                                                    <SettingEditor
                                                        setting={setting}
                                                        // Commented out to avoid re-rendering (until discovering why it was needed)
                                                        onValueChange={() =>
                                                            this.settingChanged(
                                                                setting
                                                            )
                                                        }
                                                        minLabelWidth={Math.min(
                                                            maxLabelLength * 10,
                                                            200
                                                        )}
                                                    />
                                                </Paper>
                                            </Box>
                                        ))}
                                    </Scrollable>
                                </Box>
                                {selectedSection.name === "connection" && (
                                    <BackendTester
                                        url={selectedSection.get("backend")}
                                    />
                                )}
                                {["appearance", "dark", "light"].includes(
                                    selectedSection.name
                                ) && (
                                    <Box
                                        sx={{
                                            width: "50%",
                                            height: "100%",
                                        }}
                                    >
                                        <Typography
                                            variant="subtitle1"
                                            sx={{ p: 1 }}
                                        >
                                            Code preview
                                        </Typography>
                                        <Paper
                                            variant="outlined"
                                            sx={{
                                                minWidth: 400,
                                                minHeight: 200,
                                                width: "100%",
                                                height: "30%",
                                                maxHeight: 300,
                                            }}
                                        >
                                            <CodeEditorBackend
                                                ref={(ref) => {
                                                    this.codePreviewRef = ref;
                                                }}
                                                source={this.sampleMethod()}
                                                annotations={this.sampleAnnotations()}
                                                readOnly
                                                noTooltips
                                                settings={this.props.settings}
                                                inMethod
                                                randomProp={Math.floor(
                                                    Math.random() * 100
                                                )}
                                            />
                                        </Paper>
                                    </Box>
                                )}
                            </Box>
                        </Box>
                    )}
                </CustomSplit>
                <Divider />
                <Box
                    display="flex"
                    direction="row"
                    justifyContent="flex-end"
                    mt={1}
                >
                    <Button
                        variant="outlined"
                        type="submit"
                        onClick={this.apply}
                    >
                        Apply
                    </Button>
                </Box>
            </Box>
        );
    }
}

export default SettingsEditor;
