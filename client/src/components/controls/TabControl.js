import React, { PureComponent } from "react";
import { Tabs, Tab, Box } from "@material-ui/core";
import TabPanel from "./TabPanel";
import TabLabel from "./TabLabel";

class TabControl extends PureComponent {
	tabChanged = (event, index) => {
		event.preventDefault();
		if (this.props.onSelect) {
			this.props.onSelect(this.props.pages[index]);
		}
	};

	closeTab = (event, index) => {
		event.stopPropagation();
		const page = this.props.pages[index];
		if (this.props.onClose) {
			this.props.onClose(page);
		}
	};

	render() {
		const { pages, selectedPage, noClose } = this.props;
		const selectedIndex = pages.indexOf(selectedPage);
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
					>
						{pages.map((page, index) => {
							return (
								<Tab
									component="div"
									key={index.toString()}
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
									id={`tab-${index}`}
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
								visible={page === selectedPage}
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
