import iterm2, sys

TARGET_TAB_TITLE = "devspace"

# Clear out visible terminal buffer (equivalent to Ctrl-L)
async def clear_buffer(session):
    await session.async_send_text("\f")

async def get_tab(window, tab_title):
    for tab in window.tabs:
        tab_name = await tab.async_get_variable("title")
        if tab_name == tab_title:
            return tab
            break

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

    tab = await get_tab(window, TARGET_TAB_TITLE)
    if tab is None:
        print(f"Tab with title '{TARGET_TAB_TITLE}' not found")
        sys.exit(1)

    await tab.async_activate()
    session = tab.current_session

    await clear_buffer(session)
    await session.async_send_text(f"{subcommand}\n")

iterm2.run_until_complete(main)
