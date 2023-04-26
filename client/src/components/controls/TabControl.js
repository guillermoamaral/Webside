import React, { Component } from "react";
import { Tabs, Tab, Box, IconButton, Menu, MenuItem } from "@material-ui/core";
import TabPanel from "./TabPanel";
import TabLabel from "./TabLabel";
import AddIcon from "@material-ui/icons/Add";

class TabControl extends Component {
	constructor(props) {
		super(props);
		this.state = {
			addMenuOpen: false,
		};
	}

	tabChanged = (event, index) => {
		event.preventDefault();
		if (this.props.onSelect) {
			this.props.onSelect(this.props.pages[index]);
		}
	};

	closeTab = (event, index) => {
		event.stopPropagation();
		//event.preventDefault();
		const page = this.props.pages[index];
		if (this.props.onClose) {
			this.props.onClose(page);
		}
	};

	render() {
		const { addMenuOpen } = this.state;
		const { pages, selectedPage, noClose } = this.props;
		const addOptions = this.props.addOptions || [];
		const selectedIndex = pages.findIndex((p) => p.id == selectedPage.id);
		const styles = this.props.styles;
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
										key={index.toString()}
										id={`tab-${index}`}
										style={{
											paddingTop: 1,
											paddingBotton: 0,
											minHeight: 20,
										}}
										label={
											<TabLabel
												index={index}
												icon={page.icon}
												label={page.label}
												ref={page.labelRef}
												onClose={this.closeTab}
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
							id={"addTab" + this.props.id}
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
							//id={"addTab" + this.props.id}
							anchorEl={document.getElementById(
								"addTab" + this.props.id
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
										onClick={() => {
											this.setState(
												{ addMenuOpen: false },
												option.handler
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
				</Box>
				<Box pb={0} flexGrow={1}>
					{pages.map((page, index) => {
						return (
							<TabPanel
								id={`tabpanel-${index}`}
								style={{ height: "100%" }}
								key={index.toString()}
								index={index}
								styles={styles}
								visible={page.id == selectedPage.id}
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
