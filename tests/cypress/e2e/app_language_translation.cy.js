// describe("The app's language translation", () => {
  /**
   * Cypress goes to the application's root URL. No need to specify full address 
   * because base URL is already configured in the Cypress config (tests/cypress.config.js).
   */
//   beforeEach(() => {
//     cy.visit('/')
//   })

//   it("defaults to EN in the beginning of the application, i.e. option 'en' is selected from the application start and the heading H1 is in English", () => 
//     {
//       cy.get("#app-selected_language + div")
//         .wait(3000)
//         .click()
//         .get(".selected, .option")
//         .first()
//         .should("have.text", "en")
//         .get("h1")
//         .get("#app-info-heading-second-part span")
//         .should("have.text", "Digital Twins")
//     }
//   )

//   it("switch app's language to CZ (option 'cz' is selected), and afterwards check whether H1 heading is in Czech", () => 
//     {
//       cy.get("#app-selected_language + div")
//         .wait(3000)        
//         .click({force: true})
//         .get(".items")
//         .first()
//         .click()
//         .get(".selectize-dropdown-content .option")
//         .eq(1)
//         .should("have.text", "cz")
//         .click({force: true})
//         .get("h1")
//         .get("#app-info-heading-second-part span")
//         .should("have.text", "Digitálních dvojčat")
//     }
//   )
// })
