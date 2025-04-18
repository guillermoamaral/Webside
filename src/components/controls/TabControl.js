import React, { Component } from "react";
import { Tabs, Tab, Box, IconButton, Menu, MenuItem } from "@mui/material";
import TabPanel from "./TabPanel";
import TabLabel from "./TabLabel";
import AddIcon from "@mui/icons-material/Add";
import SelectIcon from "@mui/icons-material/ExpandMore";
import CloseIcon from "@mui/icons-material/Close";
import { withTheme } from "@emotion/react";
import { darken } from "@mui/system";

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
		if (this.props.onTabSelect) {
			this.props.onTabSelect(this.props.pages[index]);
		}
	};

	tabFocused = (event, index) => {
		if (this.props.onTabFocus) {
			this.props.onTabFocus(this.props.pages[index]);
		}
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

	render() {
		const { addMenuOpen, selectMenuOpen } = this.state;
		const {
			id,
			pages,
			selectedPage,
			canCloseTabs = true,
			showClose,
			showTabSelector = true,
		} = this.props;
		const addOptions = this.props.addOptions || [];
		const selectedIndex = pages.findIndex(
			(p) => p && selectedPage && p.id === selectedPage.id
		);
		const { theme } = this.props;
		const background = theme.palette.background.default;
		return (
			<Box
				display="flex"
				flexDirection="column"
				width="100%"
				height="100%"
				maxWidth="95vw"
				onClick={() => console.log("container " + id)}
			>
				<Box
					pt={0}
					display="flex"
					flexDirection="row"
					sx={{
						overflow: "hidden",
						boxShadow: "0px 0.5px 0px rgba(128, 128, 128, 0.3)",
					}}
				>
					<Box pt={0} flexGrow={1}>
						<Tabs
							value={Math.max(selectedIndex, 0)}
							onChange={this.tabChanged}
							indicatorColor="primary"
							textColor="primary"
							variant="scrollable"
							scrollButtons="auto"
							sx={{
								py: 0,
								px: 1,
								minHeight: 20,
							}}
						>
							{pages.map((page, index) => {
								return (
									<Tab
										component="div"
										key={page.id}
										id={page.id}
										sx={{
											backgroundColor:
												index !== selectedIndex
													? darken(background, 0.05)
													: background,
											py: 0.5,
											px: 1,
											m: 0,
											minHeight: 24,
											minWidth: 32,
										}}
										onFocus={this.tabFocused}
										label={
											<TabLabel
												index={index}
												//id={"label" + page.id}
												icon={page.icon}
												label={page.label}
												ref={page.labelRef}
												onClose={this.closeTab}
												onCloseAll={this.closeAllTabs}
												onCloseOthers={
													this.closeOtherTabs
												}
												//onSplit={this.splitTab} //disabled for the moment
												showCloseOptions={canCloseTabs}
												selected={
													index === selectedIndex
												}
											/>
										}
									/>
								);
							})}
						</Tabs>
					</Box>
					{addOptions.length > 0 && (
						<Box>
							<IconButton
								id={"addTab" + id}
								onClick={() => {
									this.setState({
										addMenuOpen: true,
									});
								}}
								size="medium"
							>
								<AddIcon fontSize="small" />
							</IconButton>
							<Menu
								anchorEl={document.getElementById(
									"addTab" + id
								)}
								keepMounted
								open={addMenuOpen}
								onClose={() => {
									this.setState({
										addMenuOpen: false,
									});
								}}
							>
								{addOptions.map((option, index) => {
									return (
										<MenuItem
											key={"addOption" + index}
											onClick={(event) => {
												this.setState(
													{ addMenuOpen: false },
													() => option.handler()
												);
											}}
										>
											<Box
												display="flex"
												flexWrap="nowrap"
												alignItems="center"
												justifyContent="center"
											>
												<Box pt={1} pr={1}>
													{option.icon}
												</Box>
												<Box>{option.label}</Box>
											</Box>
										</MenuItem>
									);
								})}
							</Menu>
						</Box>
					)}
					{showTabSelector && (
						<Box>
							<IconButton
								id={"selectTab" + id}
								onClick={() => {
									this.setState({
										selectMenuOpen: true,
									});
								}}
								size="medium"
								disabled={pages.length === 0}
							>
								<SelectIcon fontSize="small" />
							</IconButton>

							<Menu
								anchorEl={document.getElementById(
									"selectTab" + id
								)}
								keepMounted
								open={selectMenuOpen}
								onClose={() => {
									this.setState({
										selectMenuOpen: false,
									});
								}}
							>
								{pages.map((page, index) => {
									return (
										<MenuItem
											key={"selectTab" + index}
											onClick={(event) => {
												this.setState(
													{ selectMenuOpen: false },
													() =>
														this.tabChanged(
															event,
															index
														)
												);
											}}
										>
											<Box
												sx={{ width: "100%" }}
												display="flex"
												flexWrap="nowrap"
												alignItems="center"
												justifyContent="space-between"
											>
												<Box
													display="flex"
													flexWrap="nowrap"
													alignItems="center"
													justifyContent="flex-start"
												>
													<Box pt={1} pr={1}>
														{page.icon}
													</Box>
													<Box>{page.label}</Box>
												</Box>
												{canCloseTabs && (
													<Box>
														<IconButton
															onClick={(
																event
															) => {
																this.closeTab(
																	event,
																	index
																);
																this.setState({
																	selectMenuOpen: false,
																});
															}}
															size="small"
														>
															<CloseIcon fontSize="small" />
														</IconButton>
													</Box>
												)}
											</Box>
										</MenuItem>
									);
								})}
							</Menu>
						</Box>
					)}
					{showClose && (
						<Box>
							<IconButton
								id={"closeAll"}
								onClick={this.closeAllTabs}
								size="medium"
							>
								<CloseIcon fontSize="small" />
							</IconButton>
						</Box>
					)}
				</Box>
				<Box pb={0} flexGrow={1}>
					{pages.map((page, index) => {
						return (
							<TabPanel
								id={id + "-" + page.id}
								key={"tab" + id + "-" + page.id}
								style={{ height: "100%" }}
								//index={index}
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
