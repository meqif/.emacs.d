import iterm2, sys

TARGET_TAB_TITLE = "devspace"

async def main(connection):
    subcommand = ' '.join(sys.argv[1:])

    app = await iterm2.async_get_app(connection)
    window = app.current_window

    if window is None:
        print("No iTerm2 window open")
        sys.exit(1)

    # Bring iTerm2 window to the foreground
    await app.async_activate()
    await window.async_activate()

    tab = None
    for _tab in window.tabs:
        tab_name = await _tab.async_get_variable("title")
        if tab_name == TARGET_TAB_TITLE:
            tab = _tab
            break

    if tab is None:
        print(f"Tab with title '{TARGET_TAB_TITLE}' not found")
        sys.exit(1)

    await tab.async_activate()
    session = tab.current_session
    await session.async_send_text(f"{subcommand}\n")

iterm2.run_until_complete(main)
