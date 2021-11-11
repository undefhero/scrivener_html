defmodule Scrivener.HTML do
  use Phoenix.HTML
  @defaults [view_style: :bootstrap, action: :index, page_param: :page, hide_single: false]
  @view_styles [:bootstrap, :semantic, :foundation, :bootstrap_v4, :materialize, :bulma]
  @raw_defaults [
    distance: 5,
    next: ">>",
    previous: "<<",
    first: true,
    last: true,
    ellipsis: raw("&hellip;")
  ]
  @moduledoc """
  For use with Phoenix.HTML, configure the `:routes_helper` module like the following:

      config :scrivener_html,
        routes_helper: MyApp.Router.Helpers

  Import to you view.

      defmodule MyApp.UserView do
        use MyApp.Web, :view
        use Scrivener.HTML
      end

  Use in your template.

      <%= pagination_links @conn, @page %>

  Where `@page` is a `%Scrivener.Page{}` struct returned from `Repo.paginate/2`.

  Customize output. Below are the defaults.

      <%= pagination_links @conn, @page, distance: 5, next: ">>", previous: "<<", first: true, last: true %>

  See `Scrivener.HTML.raw_pagination_links/2` for option descriptions.

  For custom HTML output, see `Scrivener.HTML.raw_pagination_links/2`.

  For SEO related functions, see `Scrivener.HTML.SEO` (these are automatically imported).
  """

  @doc false
  defmacro __using__(_) do
    quote do
      import Scrivener.HTML
      import Scrivener.HTML.SEO
    end
  end

  defmodule Default do
    @doc ~S"""
    Default path function when none provided. Used when automatic path function
    resolution cannot be performed.

        iex> Scrivener.HTML.Default.path(%Plug.Conn{}, :index, page: 4)
        "?page=4"
    """
    def path(_conn, _action, opts \\ []) do
      "?" <> Plug.Conn.Query.encode(opts)
    end
  end

  @doc ~S"""
  Generates the HTML pagination links for a given paginator returned by Scrivener.

  The default options are:

      #{inspect @defaults}

  The `view_style` indicates which CSS framework you are using. The default is
  `:bootstrap`, but you can add your own using the `Scrivener.HTML.raw_pagination_links/2` function
  if desired. The full list of available `view_style`s is here:

      #{inspect @view_styles}

  An example of the output data:

      iex> Scrivener.HTML.pagination_links(%Scrivener.Page{total_pages: 10, page_number: 5}) |> Phoenix.HTML.safe_to_string()
      "<nav><ul class=\"pagination\"><li class=\"\"><a class=\"\" href=\"?page=4\" rel=\"prev\">&lt;&lt;</a></li><li class=\"\"><a class=\"\" href=\"?\" rel=\"canonical\">1</a></li><li class=\"\"><a class=\"\" href=\"?page=2\" rel=\"canonical\">2</a></li><li class=\"\"><a class=\"\" href=\"?page=3\" rel=\"canonical\">3</a></li><li class=\"\"><a class=\"\" href=\"?page=4\" rel=\"prev\">4</a></li><li class=\"active\"><a class=\"\">5</a></li><li class=\"\"><a class=\"\" href=\"?page=6\" rel=\"next\">6</a></li><li class=\"\"><a class=\"\" href=\"?page=7\" rel=\"canonical\">7</a></li><li class=\"\"><a class=\"\" href=\"?page=8\" rel=\"canonical\">8</a></li><li class=\"\"><a class=\"\" href=\"?page=9\" rel=\"canonical\">9</a></li><li class=\"\"><a class=\"\" href=\"?page=10\" rel=\"canonical\">10</a></li><li class=\"\"><a class=\"\" href=\"?page=6\" rel=\"next\">&gt;&gt;</a></li></ul></nav>"

  In order to generate links with nested objects (such as a list of comments for a given post)
  it is necessary to pass those arguments. All arguments in the `args` parameter will be directly
  passed to the path helper function. Everything within `opts` which are not options will passed
  as `params` to the path helper function. For example, `@post`, which has an index of paginated
  `@comments` would look like the following:

      Scrivener.HTML.pagination_links(@conn, @comments, [@post], view_style: :bootstrap, my_param: "foo")

  You'll need to be sure to configure `:scrivener_html` with the `:routes_helper`
  module (ex. MyApp.Routes.Helpers) in Phoenix. With that configured, the above would generate calls
  to the `post_comment_path(@conn, :index, @post.id, my_param: "foo", page: page)` for each page link.

  In times that it is necessary to override the automatic path function resolution, you may supply the
  correct path function to use by adding an extra key in the `opts` parameter of `:path`.
  For example:

      Scrivener.HTML.pagination_links(@conn, @comments, [@post], path: &post_comment_path/4)

  Be sure to supply the function which accepts query string parameters (starts at arity 3, +1 for each relation),
  because the `page` parameter will always be supplied. If you supply the wrong function you will receive a
  function undefined exception.
  """
  def pagination_links(conn, paginator, args, opts) do
    opts =
      Keyword.merge(opts,
        view_style:
          opts[:view_style] || Application.get_env(:scrivener_html, :view_style, :bootstrap)
      )
      |> Keyword.merge(
        hide_single:
          opts[:hide_single] || Application.get_env(:scrivener_html, :hide_single, false)
      )

    merged_opts = Keyword.merge(@defaults, opts)

    path = opts[:path] || find_path_fn(conn && paginator.entries, args)
    params = Keyword.drop(opts, Keyword.keys(@defaults) ++ [:path, :hide_single])

    # Ensure ordering so pattern matching is reliable
    _pagination_links(paginator,
      view_style: merged_opts[:view_style],
      path: path,
      args: [conn, merged_opts[:action]] ++ args,
      page_param: merged_opts[:page_param],
      params: params
    )
  end

  def pagination_links(%Paginator.Page{} = paginator),
    do: pagination_links(nil, paginator, [], [])

  def pagination_links(%Paginator.Page{} = paginator, opts),
    do: pagination_links(nil, paginator, [], opts)

  def pagination_links(conn, %Paginator.Page{} = paginator),
    do: pagination_links(conn, paginator, [], [])

  def pagination_links(conn, paginator, [{_, _} | _] = opts),
    do: pagination_links(conn, paginator, [], opts)

  def pagination_links(conn, paginator, [_ | _] = args),
    do: pagination_links(conn, paginator, args, [])

  def find_path_fn(nil, _path_args), do: &Default.path/3
  def find_path_fn([], _path_args), do: fn _, _, _ -> nil end
  # Define a different version of `find_path_fn` whenever Phoenix is available.
  if Code.ensure_loaded(Phoenix.Naming) do
    def find_path_fn(entries, path_args) do
      routes_helper_module =
        Application.get_env(:scrivener_html, :routes_helper) ||
          raise(
            "Scrivener.HTML: Unable to find configured routes_helper module (ex. MyApp.Router.Helper)"
          )

      path = path_args |> Enum.reduce(name_for(List.first(entries), ""), &name_for/2)

      {path_fn, []} =
        Code.eval_quoted(
          quote do:
                  &(unquote(routes_helper_module).unquote(:"#{path <> "_path"}") /
                      unquote(length(path_args) + 3))
        )

      path_fn
    end
  else
    def find_path_fn(_entries, _args), do: &(Default / 3)
  end

  defp name_for(model, acc) do
    "#{acc}#{if(acc != "", do: "_")}#{Phoenix.Naming.resource_name(model.__struct__)}"
  end

  defp _pagination_links(_paginator,
         view_style: style,
         path: _path,
         args: _args,
         page_param: _page_param,
         params: _params
       )
       when not (style in @view_styles) do
    raise "Scrivener.HTML: View style #{inspect(style)} is not a valid view style. Please use one of #{
            inspect(@view_styles)
          }"
  end

  # Bootstrap implementation
  defp _pagination_links(paginator,
         view_style: :bootstrap,
         path: path,
         args: args,
         page_param: page_param,
         params: params
       ) do
    url_params = Keyword.drop(params, Keyword.keys(@raw_defaults))

    content_tag :nav do
      content_tag :ul, class: "pagination" do
        raw_pagination_links(paginator, params)
        |> Enum.map(&page(&1, url_params, args, page_param, path, paginator, :bootstrap))
      end
    end
  end

  # Bootstrap implementation
  defp _pagination_links(paginator,
         view_style: :bootstrap_v4,
         path: path,
         args: args,
         page_param: page_param,
         params: params
       ) do
    url_params = Keyword.drop(params, Keyword.keys(@raw_defaults))

    content_tag :nav, "aria-label": "Page navigation" do
      content_tag :ul, class: "pagination" do
        raw_pagination_links(paginator, params)
        |> Enum.map(&page(&1, url_params, args, page_param, path, paginator, :bootstrap_v4))
      end
    end
  end

  # Semantic UI implementation
  defp _pagination_links(paginator,
         view_style: :semantic,
         path: path,
         args: args,
         page_param: page_param,
         params: params
       ) do
    url_params = Keyword.drop(params, Keyword.keys(@raw_defaults))

    content_tag :div, class: "ui pagination menu" do
      raw_pagination_links(paginator, params)
      |> Enum.map(&page(&1, url_params, args, page_param, path, paginator, :semantic))
    end
  end

  # Foundation for Sites 6.x implementation
  defp _pagination_links(paginator,
         view_style: :foundation,
         path: path,
         args: args,
         page_param: page_param,
         params: params
       ) do
    url_params = Keyword.drop(params, Keyword.keys(@raw_defaults))

    content_tag :ul, class: "pagination", role: "pagination" do
      raw_pagination_links(paginator, params)
      |> Enum.map(&page(&1, url_params, args, page_param, path, paginator, :foundation))
    end
  end

  # Materialized implementation
  defp _pagination_links(paginator,
         view_style: :materialize,
         path: path,
         args: args,
         page_param: page_param,
         params: params
       ) do
    url_params = Keyword.drop(params, Keyword.keys(@raw_defaults))

    content_tag :ul, class: "pagination" do
      raw_pagination_links(paginator, params)
      |> Enum.map(&page(&1, url_params, args, page_param, path, paginator, :materialize))
    end
  end

  # Bulma implementation
  defp _pagination_links(paginator,
         view_style: :bulma,
         path: path,
         args: args,
         page_param: page_param,
         params: params
       ) do
    url_params = Keyword.drop(params, Keyword.keys(@raw_defaults))

    content_tag :nav, class: "pagination is-centered" do
      content_tag :ul, class: "pagination-list" do
        raw_pagination_links(paginator, params)
        |> Enum.map(&page(&1, url_params, args, page_param, path, paginator, :bulma))
      end
    end
  end

  defp page({:ellipsis, true}, url_params, args, page_param, path, paginator, :foundation) do
    page({:ellipsis, ""}, url_params, args, page_param, path, paginator, :foundation)
  end

  defp page({:ellipsis, true}, url_params, args, page_param, path, paginator, style) do
    page(
      {:ellipsis, unquote(@raw_defaults[:ellipsis])},
      url_params,
      args,
      page_param,
      path,
      paginator,
      style
    )
  end

  defp page({:ellipsis, text}, _url_params, _args, _page_param, _path, paginator, :semantic) do
    content_tag(:div, safe(text),
      class: link_classes_for_style(paginator, :ellipsis, :semantic) |> Enum.join(" ")
    )
  end

  defp page({:ellipsis, text} = value, _url_params, _args, _page_param, _path, paginator, style) do
    content_tag(:li, class: ["disabled"] ++ li_classes_for_style(paginator, :ellipsis, style) |> Enum.join(" ")) do
      style
      |> ellipsis_tag
      |> content_tag(safe(text),
        class: link_classes_for_style(paginator, :ellipsis, style) |> Enum.join(" ")
      )
    end
  end

  defp page({">>", cursor} = value, url_params, args, _page_param, path, paginator, style) do
    params_with_cursor = url_params ++ [{:after, cursor}]
    apply_page(value, url_params, args, params_with_cursor, path, paginator, style)
  end

  defp page({"<<", cursor} = value, url_params, args, page_param, path, paginator, style) do
    params_with_cursor = url_params ++ [{:before, cursor}]
    apply_page(value, url_params, args, params_with_cursor, path, paginator, style)
  end

  defp page(value, url_params, args, page_param, path, paginator, style) do
    apply_page(value, url_params, args, page_param, path, paginator, style)
  end

  defp apply_page({text, page_number} = value, url_params, args, params_with_cursor, path, paginator, :semantic) do
    to = apply(path, args ++ [params_with_cursor])

    if to do
      link(safe(text),
        to: to,
        rel: Scrivener.HTML.SEO.rel(paginator, page_number),
        class: li_classes_for_style(paginator, page_number, :semantic) |> Enum.join(" ")
      )
    else
      content_tag(:a, safe(text),
        class: li_classes_for_style(paginator, page_number, :semantic) |> Enum.join(" ")
      )
    end
  end

  defp apply_page({text, page_number} = value, url_params, args, params_with_cursor, path, paginator, style) do
    content_tag :li, class: li_classes_for_style(paginator, page_number, style) |> Enum.join(" ") do
      to = apply(path, args ++ [params_with_cursor])

      if to do
        link(safe(text),
          to: to,
          rel: Scrivener.HTML.SEO.rel(paginator, page_number),
          class: link_classes_for_style(paginator, page_number, style) |> Enum.join(" ")
        )
      else
        style
        |> blank_link_tag()
        |> content_tag(safe(text),
          class: link_classes_for_style(paginator, page_number, style) |> Enum.join(" ")
        )
      end
    end
  end

  defp li_classes_for_style(_paginator, :ellipsis, :bootstrap), do: []

  defp li_classes_for_style(paginator, _page_number, :bootstrap) do
    ["active"]
  end

  defp li_classes_for_style(_paginator, :ellipsis, :bootstrap_v4), do: ["page-item"]

  defp li_classes_for_style(paginator, _page_number, :bootstrap_v4) do
    ["page-item"]
  end

  defp li_classes_for_style(_paginator, :ellipsis, :foundation), do: ["ellipsis"]

  defp li_classes_for_style(paginator, _page_number, :foundation) do
    []
  end

  defp li_classes_for_style(_paginator, :ellipsis, :semantic), do: ["ellipsis"]

  defp li_classes_for_style(paginator, _page_number, :semantic) do
    ["item"]
  end

  defp li_classes_for_style(_paginator, :ellipsis, :materialize), do: []

  defp li_classes_for_style(paginator, _page_number, :materialize) do
    ["waves-effect"]
  end

  defp li_classes_for_style(_paginator, :ellipsis, :bulma), do: []
  defp li_classes_for_style(_paginator, _page_number, :bulma), do: []

  defp link_classes_for_style(_paginator, :ellipsis, :semantic), do: ["disabled", "item"]
  defp link_classes_for_style(_paginator, :ellipsis, :materialize), do: []
  defp link_classes_for_style(_paginator, :ellipsis, :bulma), do: ["pagination-ellipsis"]
  defp link_classes_for_style(_paginator, _page_number, :bootstrap), do: []
  defp link_classes_for_style(_paginator, _page_number, :bootstrap_v4), do: ["page-link"]
  defp link_classes_for_style(_paginator, _page_number, :foundation), do: []

  defp link_classes_for_style(paginator, page_number, :semantic) do
    if(paginator.page_number == page_number, do: ["active", "item"], else: ["item"])
  end

  defp link_classes_for_style(_paginator, _page_number, :materialize), do: []

  defp link_classes_for_style(paginator, page_number, :bulma) do
    if(paginator.page_number == page_number,
      do: ["pagination-link", "is-current"],
      else: ["pagination-link"]
    )
  end

  defp ellipsis_tag(:semantic), do: :div
  defp ellipsis_tag(_), do: :span

  defp blank_link_tag(:foundation), do: :span
  defp blank_link_tag(_), do: :a

  @doc """
  Returns the raw data in order to generate the proper HTML for pagination links. Data
  is returned in a `{text, page_number}` format where `text` is intended to be the text
  of the link and `page_number` is the page it should go to. Defaults are already supplied
  and they are as follows:

      #{inspect(@raw_defaults)}

  `distance` must be a positive non-zero integer or an exception is raised. `next` and `previous` should be
  strings but can be anything you want as long as it is truthy, falsey values will remove
  them from the output. `first` and `last` are only booleans, and they just include/remove
  their respective link from output. An example of the data returned:

      iex> Scrivener.HTML.raw_pagination_links(%{total_pages: 10, page_number: 5})
      [{"<<", 4}, {1, 1}, {2, 2}, {3, 3}, {4, 4}, {5, 5}, {6, 6}, {7, 7}, {8, 8}, {9, 9}, {10, 10}, {">>", 6}]
      iex> Scrivener.HTML.raw_pagination_links(%{total_pages: 20, page_number: 10}, first: ["←"], last: ["→"])
      [{"<<", 9}, {["←"], 1}, {:ellipsis, {:safe, "&hellip;"}}, {5, 5}, {6, 6},{7, 7}, {8, 8}, {9, 9}, {10, 10}, {11, 11}, {12, 12}, {13, 13}, {14, 14},{15, 15}, {:ellipsis, {:safe, "&hellip;"}}, {["→"], 20}, {">>", 11}]

  Simply loop and pattern match over each item and transform it to your custom HTML.
  """
  def raw_pagination_links(paginator, options \\ []) do
    options = Keyword.merge(@raw_defaults, options)

    add_first(paginator.metadata)
    |> add_ellipsis()
    |> add_last()
    |> Enum.map(fn
      {:next, next_cursor} ->
        if options[:next], do: {options[:next], next_cursor}

      {:previous, previous_cursor} ->
        if options[:previous], do: {options[:previous], previous_cursor}

      :ellipsis ->
        if options[:ellipsis] && options[:first], do: {:ellipsis, options[:ellipsis]}
    end)
    |> Enum.filter(& &1)
  end

  defp add_first(%{before: nil} = metadata) do
    {[], metadata}
  end

  defp add_first(%{before: previous_cursor} = metadata) do
    {[{:previous, previous_cursor}], metadata}
  end

  defp add_ellipsis({list, metadata}) do
    {list ++ [:ellipsis], metadata}
  end

  defp add_last({list, %{after: nil} = metadata}) do
    list
  end

  defp add_last({list, %{after: next_cursor} = metadata}) do
    list ++ [{:next, next_cursor}]
  end

  defp safe({:safe, _string} = whole_string) do
    whole_string
  end

  defp safe(string) when is_binary(string) do
    string
  end

  defp safe(string) do
    string
    |> to_string()
    |> raw()
  end

  def defaults(), do: @defaults
end
