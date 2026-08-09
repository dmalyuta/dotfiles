#!/usr/bin/env python3
"""Direction-aware window resizing, mapped to ctrl+arrow in kitty.conf.

kitty's built-in `resize_window wider|narrower|taller|shorter` is relative to
the focused window, so the same key grows the window in one split and shrinks
it in another: `narrower` drags the right border left when the window sits on
the left of a split, but drags the *left* border right when it sits on the
right. This kitten instead asks the layout where the neighbours are and picks
the quality that always drags a border in the direction of the arrow key.

When the window has neighbours on both sides, the border on the right/bottom
of it is the one that moves, so left/right (and up/down) stay reversible.

Usage:  map ctrl+left kitten resize_split.py left [cells]
"""

from typing import TYPE_CHECKING, Any, List

if TYPE_CHECKING:
    from kitty.boss import Boss

# The border that moves is the one on this side of the window, when it exists.
ANCHOR = {'left': 'right', 'right': 'right', 'up': 'bottom', 'down': 'bottom'}
DEFAULT_CELLS = 2


def main(args: List[str]) -> None:
    # Never runs: handle_result.no_ui tells kitty to skip the kitten window.
    pass


def handle_result(args: List[str], answer: Any, target_window_id: int, boss: 'Boss') -> None:
    direction = args[1]
    anchor = ANCHOR.get(direction)
    if anchor is None:
        return
    cells = int(args[2]) if len(args) > 2 else DEFAULT_CELLS
    tab = boss.active_tab
    if tab is None:
        return

    # Is there a window between this one and the right/bottom edge of the tab?
    # If so that shared border is the one to drag; otherwise the window is
    # flush against the edge and the border on its other side has to move,
    # which flips the sense of the resize.
    has_trailing_neighbor = tab.neighboring_group_id(anchor) is not None
    grow = (direction in ('right', 'down')) == has_trailing_neighbor
    if direction in ('left', 'right'):
        quality = 'wider' if grow else 'narrower'
    else:
        quality = 'taller' if grow else 'shorter'
    tab.resize_window(quality, cells)


handle_result.no_ui = True  # type: ignore[attr-defined]
