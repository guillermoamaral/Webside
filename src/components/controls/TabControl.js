import { Component } from "react";
import {
    Tabs,
    Tab,
    Box,
    IconButton,
    Menu,
    MenuItem,
    Typography,
} from "@mui/material";
import TabPanel from "./TabPanel";
import TabLabel from "./TabLabel";
import AddIcon from "@mui/icons-material/Add";
import SelectIcon from "@mui/icons-material/ExpandMore";
import CloseIcon from "@mui/icons-material/Close";
import { darken } from "@mui/system";
import DraggableReorderItem from "../controls/DraggableReorderItem";
import { withTheme } from "@emotion/react";
import ShortcutLegend from "../parts/ShortcutLegend";

class TabControl extends Component {
    constructor(props) {
        super(props);
        this.state = {
            addMenuOpen: false,
            selectMenuOpen: false,
        };
    }

    tabChanged = (event, index) => {
        event.preventDefault();
        if (this.props.onTabSelect)
            this.props.onTabSelect(this.props.pages[index]);
    };

    tabFocused = (event, index) => {
        if (this.props.onTabFocus)
            this.props.onTabFocus(this.props.pages[index]);
    };

    closeTab = (event, index) => {
        if (event) {
            event.stopPropagation();
            event.preventDefault();
        }
        const page = this.props.pages[index];
        if (this.props.onTabsClose) {
            this.props.onTabsClose([page]);
        }
    };

    splitTab = (index) => {
        const page = this.props.pages[index];
        if (this.props.onTabSplit) {
            this.props.onTabSplit(page);
        }
    };

    closeAllTabs = () => {
        if (this.props.onTabsClose) {
            this.props.onTabsClose(this.props.pages);
        }
    };

    closeOtherTabs = (index) => {
        if (this.props.onTabsClose) {
            const others = this.props.pages.filter((p, i) => i !== index);
            this.props.onTabsClose(others);
        }
    };

    moveTab = (from, to) => {
        if (this.props.onTabsReorder) this.props.onTabsReorder(from, to);
    };

    render() {
        const { addMenuOpen, selectMenuOpen } = this.state;
        const {
            id,
            pages,
            selectedPage,
            canCloseTabs = true,
            showClose,
            showTabSelector = true,
            addOptions = [],
        } = this.props;
        const { theme } = this.props;
        const background = theme.palette.background.default;
        const selectedIndex = pages.findIndex(
            (p) => p && selectedPage && p.id === selectedPage.id
        );
        return (
            <Box
                display="flex"
                flexDirection="column"
                width="100%"
                height="100%"
                //onClick={() => console.log("container " + id)}
            >
                <Box
                    pt={0}
                    sx={{
                        //boxShadow: "0px 0.5px 0px rgba(128, 128, 128, 0.3)",
                        minHeight: 32,
                        overflow: "hidden",
                        display: "flex",
                        alignItems: "center",
                        width: "100%",
                        flexDirection: "row",
                    }}
                >
                    <Box sx={{ flexGrow: 1, minWidth: 0 }}>
                        <Tabs
                            value={Math.max(selectedIndex, 0)}
                            onChange={this.tabChanged}
                            variant="scrollable"
                            scrollButtons="auto"
                            indicatorColor="primary"
                            textColor="primary"
                            sx={{
                                minHeight: 32,
                                py: 0,
                                px: 1,
                            }}
                        >
                            {pages.map((page, index) => (
                                <Tab
                                    component="div"
                                    key={page.id}
                                    id={page.id}
                                    sx={{
                                        backgroundColor:
                                            index !== selectedIndex
                                                ? darken(background, 0.15)
                                                : background,
                                        py: 0.5,
                                        px: 1,
                                        m: 0,
                                        minHeight: 24,
                                        minWidth: 32,
                                    }}
                                    onFocus={this.tabFocused}
                                    label={
                                        <DraggableReorderItem
                                            key={page.id}
                                            index={index}
                                            type="TAB"
                                            onMove={this.moveTab}
                                            direction="horizontal"
                                            sx={{
                                                display: "inline-flex",
                                                alignItems: "center",
                                            }}
                                        >
                                            <TabLabel
                                                index={index}
                                                icon={page.icon}
                                                label={page.label}
                                                ref={page.labelRef}
                                                onClose={this.closeTab}
                                                onCloseAll={this.closeAllTabs}
                                                onCloseOthers={() =>
                                                    this.closeOtherTabs(index)
                                                }
                                                showCloseOptions={canCloseTabs}
                                                selected={
                                                    index === selectedIndex
                                                }
                                                tooltip={page.description}
                                            />
                                        </DraggableReorderItem>
                                    }
                                />
                            ))}
                        </Tabs>
                    </Box>
                    <Box
                        sx={{
                            display: "flex",
                            flexShrink: 0,
                            alignItems: "center",
                            px: 1,
                            backgroundColor: background,
                        }}
                    >
                        {addOptions.length > 0 && (
                            <IconButton
                                id={"addTab" + id}
                                onClick={() =>
                                    this.setState({ addMenuOpen: true })
                                }
                                size="medium"
                            >
                                <AddIcon fontSize="small" />
                            </IconButton>
                        )}
                        {showTabSelector && (
                            <IconButton
                                id={"selectTab" + id}
                                onClick={() =>
                                    this.setState({ selectMenuOpen: true })
                                }
                                size="medium"
                                disabled={pages.length === 0}
                            >
                                <SelectIcon fontSize="small" />
                            </IconButton>
                        )}
                        {showClose && (
                            <IconButton
                                id={"closeAll"}
                                onClick={this.closeAllTabs}
                                size="medium"
                            >
                                <CloseIcon fontSize="small" />
                            </IconButton>
                        )}
                    </Box>
                    <Menu
                        anchorEl={document.getElementById("addTab" + id)}
                        keepMounted
                        open={addMenuOpen}
                        onClose={() => this.setState({ addMenuOpen: false })}
                    >
                        {addOptions.map((option, index) => (
                            <MenuItem
                                key={"addOption" + index}
                                onClick={() =>
                                    this.setState({ addMenuOpen: false }, () =>
                                        option.handler()
                                    )
                                }
                            >
                                <Box
                                    display="flex"
                                    alignItems="center"
                                    width="100%"
                                >
                                    <Box pt={1} pr={1}>
                                        {option.icon}
                                    </Box>
                                    <Box
                                        sx={{
                                            display: "flex",
                                            justifyContent: "space-between",
                                            alignItems: "baseline",
                                            width: "100%",
                                        }}
                                    >
                                        <Typography>{option.label}</Typography>
                                        {option.shortcut && (
                                            <Box ml={1}>
                                                <ShortcutLegend
                                                    shortcut={option.shortcut}
                                                />
                                            </Box>
                                        )}
                                    </Box>
                                </Box>
                            </MenuItem>
                        ))}
                    </Menu>
                    <Menu
                        anchorEl={document.getElementById("selectTab" + id)}
                        keepMounted
                        open={selectMenuOpen}
                        onClose={() => this.setState({ selectMenuOpen: false })}
                    >
                        {pages.map((page, index) => (
                            <MenuItem
                                key={"selectTab" + index}
                                onClick={(event) =>
                                    this.setState(
                                        { selectMenuOpen: false },
                                        () => this.tabChanged(event, index)
                                    )
                                }
                            >
                                <Box
                                    sx={{ width: "100%" }}
                                    display="flex"
                                    alignItems="center"
                                    justifyContent="space-between"
                                >
                                    <Box display="flex" alignItems="center">
                                        <Box pt={1} pr={1}>
                                            {page.icon}
                                        </Box>
                                        <Box>{page.label}</Box>
                                    </Box>
                                    {canCloseTabs && (
                                        <IconButton
                                            onClick={(event) => {
                                                this.closeTab(event, index);
                                                this.setState({
                                                    selectMenuOpen: false,
                                                });
                                            }}
                                            size="small"
                                        >
                                            <CloseIcon fontSize="small" />
                                        </IconButton>
                                    )}
                                </Box>
                            </MenuItem>
                        ))}
                    </Menu>
                </Box>
                <Box pb={0} flexGrow={1}>
                    {pages.map((page) => {
                        return (
                            <TabPanel
                                id={id + "-" + page.id}
                                key={"tab" + id + "-" + page.id}
                                style={{ height: "100%" }}
                                visible={
                                    selectedPage !== undefined &&
                                    page.id === selectedPage.id
                                }
                            >
                                {page.component}
                            </TabPanel>
                        );
                    })}
                </Box>
            </Box>
        );
    }
}

export default withTheme(TabControl);
