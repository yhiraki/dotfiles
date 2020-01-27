<template>`(evil-insert-state)`
</template>

<script lang="${1:js}">
${1:$(when (equal yas-text "js")
"export default {
  name: '',

  components: {},

  data: () => ({}),

  computed: {},

  methods: {},

  mounted() {}
}")}${1:$(when (equal yas-text "ts")
"import { Vue, Component } from 'vue-property-decorator'

@Component
export default class CassName extends Vue {
}")}
</script>

<style scoped>
</style>
