import React, { useRef, useEffect } from "react";
import { useDrag, useDrop } from "react-dnd";
import { Box } from "@mui/material";
import { getEmptyImage } from "react-dnd-html5-backend";

const DraggableReorderItem = ({
	index,
	type,
	onMove,
	direction = "none", // 'vertical' | 'horizontal' | 'none'
	dragHandle = false, // if false, drag cursor on whole area
	children,
	sx = {},
	showPreview = false,
	onDraggingChange = () => {},
}) => {
	const wrapperRef = useRef(null);
	const handleRef = useRef(null);
	const lastDragging = useRef(false);

	const [, drop] = useDrop({
		accept: type,
		hover(item, monitor) {
			if (!wrapperRef.current || item.index === index) return;

			if (direction === "none") {
				onMove(item.index, index);
				item.index = index;
				return;
			}

			const boundingRect = wrapperRef.current.getBoundingClientRect();
			const middle =
				direction === "vertical"
					? (boundingRect.bottom - boundingRect.top) / 2
					: (boundingRect.right - boundingRect.left) / 2;

			const clientOffset = monitor.getClientOffset();
			if (!clientOffset) return;

			const offset =
				direction === "vertical"
					? clientOffset.y - boundingRect.top
					: clientOffset.x - boundingRect.left;

			const isMovingDownOrRight = item.index < index && offset < middle;
			const isMovingUpOrLeft = item.index > index && offset > middle;

			if (isMovingDownOrRight || isMovingUpOrLeft) return;

			onMove(item.index, index);
			item.index = index;
		},
	});

	const [{ isDragging }, drag, preview] = useDrag({
		type,
		item: { index },
		collect: (monitor) => ({
			isDragging: monitor.isDragging(),
		}),
	});

	useEffect(() => {
		if (lastDragging.current !== isDragging) {
			lastDragging.current = isDragging;
			onDraggingChange(isDragging);
		}
	}, [isDragging, onDraggingChange]);

	useEffect(() => {
		if (showPreview) {
			if (wrapperRef.current) {
				preview(wrapperRef.current);
			}
		} else {
			preview(getEmptyImage(), { captureDraggingState: true });
		}
	}, [preview]);

	if (dragHandle && handleRef.current) {
		drag(handleRef);
	} else {
		drag(drop(wrapperRef));
	}
	if (dragHandle) drop(wrapperRef);

	return (
		<Box
			ref={wrapperRef}
			sx={{
				opacity: isDragging ? 0.5 : 1,
				...sx,
			}}
		>
			{dragHandle && (
				<Box
					ref={handleRef}
					sx={{
						width: 16,
						cursor: "move",
					}}
				/>
			)}
			<Box sx={{ flexGrow: 1 }}>{children}</Box>
		</Box>
	);
};

export default DraggableReorderItem;
