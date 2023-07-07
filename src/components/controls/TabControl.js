import React, { Component } from "react";
import { Tabs, Tab, Box, IconButton, Menu, MenuItem } from "@mui/material";
import TabPanel from "./TabPanel";
import TabLabel from "./TabLabel";
import AddIcon from "@mui/icons-material/Add";
import SelectIcon from "@mui/icons-material/ExpandMore";
import CloseIcon from "@mui/icons-material/Close";

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
		const { id, pages, selectedPage, noClose } = this.props;
		const addOptions = this.props.addOptions || [];
		const selectedIndex = pages.findIndex((p) => p.id === selectedPage.id);
		return (
			<Box
				display="flex"
				flexDirection="column"
				width="100%"
				height="100%"
			>
				<Box pt={0} display="flex" flexDirection="row">
					<Box pt={0} width="95%">
						<Tabs
							value={Math.max(selectedIndex, 0)}
							onChange={this.tabChanged}
							indicatorColor="primary"
							textColor="primary"
							variant="scrollable"
							scrollButtons="auto"
							style={{
								paddingTop: 1,
								paddingBotton: 0,
								minHeight: 20,
							}}
						>
							{pages.map((page, index) => {
								return (
									<Tab
										component="div"
										key={page.id}
										id={page.id}
										style={{
											paddingTop: 1,
											paddingBotton: 0,
											minHeight: 20,
										}}
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
												onSplit={this.splitTab}
												noClose={noClose}
											/>
										}
									/>
								);
							})}
						</Tabs>
					</Box>
					<Box flexShrink={0}>
						<IconButton
							id={"addTab" + id}
							onClick={() => {
								this.setState({
									addMenuOpen: true,
								});
							}}
							size="medium"
						>
							<AddIcon />
						</IconButton>
						<Menu
							//id={"addTab" + id}
							anchorEl={document.getElementById("addTab" + id)}
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
					<Box flexShrink={0}>
						<IconButton
							id={"selectTab" + id}
							onClick={() => {
								this.setState({
									selectMenuOpen: true,
								});
							}}
							size="medium"
						>
							<SelectIcon />
						</IconButton>
						<Menu
							//id={"addTab" + id}
							anchorEl={document.getElementById("selectTab" + id)}
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
											<Box>
												<IconButton
													onClick={(event) =>
														this.closeTab(
															event,
															index
														)
													}
													size="small"
												>
													<CloseIcon fontSize="small" />
												</IconButton>
											</Box>
										</Box>
									</MenuItem>
								);
							})}
						</Menu>
					</Box>
				</Box>
				<Box pb={0} flexGrow={1}>
					{pages.map((page, index) => {
						return (
							<TabPanel
								id={id + "-" + page.id}
								key={"tab" + id + "-" + page.id}
								style={{ height: "100%" }}
								//index={index}
								visible={page.id === selectedPage.id}
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

export default TabControl;
