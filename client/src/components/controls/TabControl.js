import React, { Component } from "react";
import { Tabs, Tab, Box } from "@material-ui/core";
import TabPanel from "./TabPanel";
import TabLabel from "./TabLabel";

class TabControl extends Component {
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
		const { pages, selectedPage, noClose } = this.props;
		const selectedIndex = pages.findIndex((p) => p.id == selectedPage.id);
		const styles = this.props.styles;
		return (
			<Box
				display="flex"
				flexDirection="column"
				width="100%"
				height="100%"
			>
				<Box pt={0}>
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
