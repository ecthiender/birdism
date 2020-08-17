class AutoComplete {
  view(vnode) {
    const {results, selected} = vnode.attrs;
    if (!results.length) {
      return null;
    }
    const items = results.map(item => m("div.item", {onclick: (e) => {
      console.log('clicked item', item);
      selected(item);
    }}, item.elem));
    return m("div.autocomplete-form", items);
  }
}

class SearchForm {
  constructor() {
    this.current = {
      family: '',
      typingFamily: '',
      region: '',
      typingRegion: '',
      matchingFamily: [],
      matchingRegion: [],
    };
  }
  handleSubmit (app) {
    return (e) => {
      e.preventDefault();
      // console.log(e);
      // console.log(this.current);
      this.handleSearch(app, this.current);
    };
  }

  handleSearch (app, query) {
    app.errors = [];

    if (query.family == '' || query.region == '') {
      app.errors.push('Enter family and region to search');
      return;
    }

    app.results = [];
    app.searching = true;

    // console.log('making request: ', query);
    m.request({
      method: "POST",
      url: `/api/v1/search`,
      body: {
        "family": query.family.key,
        "region": query.region.key
      }
    })
    .then((result) => {
      console.log('search results', result);
      app.results = result.result;
      app.searching = false;
    })
    .catch((err) => {
      console.log('search error', err);
      app.searching = false;
      app.errors.push(err);
    });
  }

  handleFamilyAutocomplete(term) {
    if (term === "") {
      this.current.matchingFamily = [];
      return;
    }
    const results = app.familyNames.filter(n => n.common_name.toLowerCase().indexOf(term) >= 0)
          .map(n => { return {
            elem: m(FamilyName, {...n}),
            key: n.scientific_name,
            text: n.common_name + " (" + n.scientific_name + ")"
          }; });
    console.log('auto complete res', results);
    this.current.matchingFamily = results;
  }

  handleRegionAutocomplete(term) {
    if (term === "") {
      this.current.matchingRegion = [];
      return;
    }
    const results = app.regions.filter(n => n.region_name.toLowerCase().indexOf(term) >= 0)
          .map(n => { return {elem: m("span", n.region_name), key: n.region_code, text: n.region_name}; });
    console.log('auto complete res', results);
    this.current.matchingRegion = results;
  }

  view (vnode) {
    return m("div", [
      m("form.form-inline", {onsubmit: this.handleSubmit(vnode.attrs.app).bind(this)}, [
        m("div.row", [
          m("div.col", [
            m("input.form-control[type=text][placeholder=Enter a family (flycatcher)]", {
              oninput: (event) => {
                console.log('oninput called', event.target.value);
                vnode.state.current.family = '';
                vnode.state.current.typingFamily = event.target.value;
                this.handleFamilyAutocomplete(event.target.value);
              },
              // when the family input box loses focus, remove the autocomplete
              onblur: () => { setTimeout(() => {vnode.state.current.matchingFamily = [];}, 10); },
              value: vnode.state.current.family ?
                // (vnode.state.current.family.common_name + " (" + vnode.state.current.family.scientific_name + ")") :
                vnode.state.current.family.text :
                vnode.state.current.typingFamily
            }),
            m(AutoComplete, {
              results: vnode.state.current.matchingFamily,
              selected: (family) => {
                console.log('selecting family', family);
                vnode.state.current.family = family;
                console.log('updated state', vnode.state.current);
                // m.redraw();
              }
            })
          ]),
          m("div.col", [
            m("input.form-control[type=text][placeholder=Enter a region (bangalore)]", {
              oninput: (event) => {
                console.log(event.target.value);
                vnode.state.current.region = '';
                vnode.state.current.typingRegion = event.target.value;
                this.handleRegionAutocomplete(event.target.value);
              },
              // when the region input box loses focus, remove the autocomplete
              onblur: () => { setTimeout(() => {vnode.state.current.matchingRegion = [];}, 10); },
              value: vnode.state.current.region ? vnode.state.current.region.text : vnode.state.current.typingRegion
            }),
            m(AutoComplete, {
              results: vnode.state.current.matchingRegion,
              selected: (region) => {
                console.log('selecting region', region);
                vnode.state.current.region = region;
                console.log('updated state', vnode.state.current);
                // m.redraw();
              }
            })
          ]),
          m("div.col",
            m("button.btn.btn-primary[type=submit]", {
              disabled: vnode.attrs.app.searching
            }, "Search")
           )
        ]),
      ]),
      m("hr")
    ]);
  }
}

class BirdPhoto {
  view (vnode) {
    const {photos, species} = vnode.attrs;
    console.log(photos);
    const res = photos.map((photo) => {
      return m("img", {src: photo});
    });
    return m("div.card", [
      // m("img.card-img-top", {src: photos[0][0], height: photos[0][1], width: photos[0][1]}),
      m("div.card-body", [
        m("h3.card-title", species),
        ...res
      ])
    ]);
  }
}

class BirdList {
  view (vnode) {
    const res = vnode.attrs.result;
    console.log('Redering BirdList', res);
    if (res.length <= 0) {
      return null;
    }
    return m("div", [
      m("div.alert.alert-info", `${res.length} species found.`),
      ...res.map(sp => m(BirdPhoto, {species: sp.common_name, photos: sp.image_urls}))
    ]);
  }
}

class Errors {
  view (vnode) {
    const errs = vnode.attrs.errors.map((err) => {
      let msg = typeof(err) === "string" ? err : err.toString();
      return m("div.alert.alert-danger", msg);
    });
    return m("div", errs);
  }
}

class App {
  constructor () {
    // app state
    this.results = [];
    this.searching = false;
    this.errors = [];
    this.familyNames = [];
    this.regions = [];
  }

  oninit() {
    this.loadFamilyNames();
    this.loadRegions();
  }

  loadFamilyNames () {
    m.request({
      method: "GET",
      url: `/api/v1/families`,
    })
    .then((result) => {
      console.log('family results', result);
      app.familyNames = result;
    })
    .catch((err) => {
      console.log('error fetching family names', err);
      app.errors.push(err);
    });
  }

  loadRegions () {
    m.request({
      method: "GET",
      url: `/api/v1/regions`,
    })
      .then((result) => {
        // console.log('region results', result);
        // app.regions = result;
        app.regions = result;
      })
      .catch((err) => {
        console.log('error fetching family names', err);
        app.errors.push(err);
      });
  }

  view (vnode) {
    return m("div", [
      m(SearchForm, {app: vnode.state}),
      vnode.state.searching ? m("div.alert.alert-info.col-sm-6", "Searching..") : null,
      m(Errors, {errors: vnode.state.errors}),
      m(BirdList, {result: vnode.state.results})
    ]);
  }
}

const FamilyName = {
  view: function(vnode) {
    return m("span", [m("span", vnode.attrs.common_name),
                      m("small", m("i", " (" + vnode.attrs.scientific_name + ")"))
                     ]);
  }
};

// const mkAutoItem = (n) => m("span",
//                             [ m("span", n.common_name),
//                               m("small", m("i", " (" + n.scientific_name + ")"))
//                             ]);

