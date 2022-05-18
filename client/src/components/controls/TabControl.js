import React, { PureComponent } from "react";
import { Tabs, Tab, Box } from "@material-ui/core";
import TabPanel from "./TabPanel";
import TabLabel from "./TabLabel";

class TabControl extends PureComponent {
	tabChanged = (event, index) => {
		event.preventDefault();
		const handler = this.props.onSelect;
		if (handler) {
			handler(this.props.pages[index]);
		}
	};

	closeTab = (event, index) => {
		event.stopPropagation();
		const page = this.props.pages[index];
		const handler = this.props.onClose;
		if (handler) {
			handler(page);
		}
	};

	render() {
		const { pages, selectedPage } = this.props;
		const selectedIndex = pages.indexOf(selectedPage);
		const styles = this.props.styles;
		return (
			<Box flexGrow={1} width="100%" height="100%">
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
										onClose={this.closeTab}
									/>
								}
								id={`tab-${index}`}
							/>
						);
					})}
				</Tabs>
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
		);
	}
}

export default TabControl;
