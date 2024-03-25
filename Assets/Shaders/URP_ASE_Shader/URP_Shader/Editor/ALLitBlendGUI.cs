using System.Runtime.CompilerServices;
using UnityEditor;
using UnityEngine;

public class ALLitBlendGUI : ShaderGUI
{
    public enum BlendMode
    {
        Opaque,
        Cutout,
        Transparent,
        PreMultiply,
    }
    public enum CullMode
    {
        Back,
        Front,
        Double,
    }
    public enum EmissFormat
    {
        Mask,
        Map,
    }
    public enum EmissFormat3
    {
        Mask,
        Map,
    }

    private static class Styles
    {

        //Enum
        public static string renderingMode = "RenderingMode";
        public static string cullingMode = "CullingMode";
        public static string emissformat = "EmissFormat";
        public static string emissformat3 = "EmissFormat";

        public static readonly string[] blendNames = { "Opaque", "Cutout", "Transparent", "PreMultiply" };
        public static readonly string[] cullNames = { "Front", "Back", "Double" };
        public static readonly string[] emissNames = { "Mask", "Map" };
        public static readonly string[] emissNames3 = { "Mask", "Map" };
        // texture
        public static GUIContent baseMapText = new GUIContent("Albdo");
        public static GUIContent baseMapText3 = new GUIContent("Albdo(Blend)");
        public static GUIContent normalMapText = new GUIContent("Normal");
        public static GUIContent normalMapText3 = new GUIContent("Normal");
        public static GUIContent msaeMapTex = new GUIContent("Meta(R) Gloss(G) AO(B) Emiss(A)");
        public static GUIContent msaeMapTex3 = new GUIContent("Meta(R) Gloss(G) AO(B) Emiss(A)");
        public static GUIContent emissMapTex = new GUIContent("EmissionMap");
        public static GUIContent emissMapTex3 = new GUIContent("EmissionMap");
        public static GUIContent smoothnessRemapText = new GUIContent("GlossRef");
        public static GUIContent smoothnessRemapText3 = new GUIContent("GlossRef");
        public static GUIContent queueSlider = new GUIContent("Priority");

        public static GUIContent maskMapTex = new GUIContent("BlendMask(A)");
    }

    //enum
    MaterialProperty blendMode = null;
    MaterialProperty cullMode = null;
    MaterialProperty emissMode = null;
    MaterialProperty emissMode3 = null;
    //
    MaterialProperty baseMap = null;
    MaterialProperty baseMap3 = null;
    MaterialProperty baseColor = null;
    MaterialProperty baseColor3 = null;
    MaterialProperty normalMap = null;
    MaterialProperty normalMap3 = null;
    MaterialProperty normalScale = null;
    MaterialProperty normalScale3 = null;
    MaterialProperty maseMap = null;
    MaterialProperty maseMap3 = null;
    MaterialProperty emissMap = null;
    MaterialProperty emissMap3 = null;
    MaterialProperty emissiveColor = null;
    MaterialProperty emissiveColor3 = null;
    MaterialProperty smoothnessMin = null;
    MaterialProperty smoothnessMin3 = null;
    MaterialProperty smoothnessMax = null;
    MaterialProperty smoothnessMax3 = null;
    MaterialProperty metallic = null;
    MaterialProperty metallic3 = null;
    MaterialProperty AOStrength = null;
    MaterialProperty AOStrength3 = null;
    MaterialProperty cutoff = null;
    MaterialProperty QueueOffset = null;
    MaterialProperty transparentZWrite = null;
    //Blend Mode
    MaterialProperty BlendMaskMap = null;
    MaterialProperty MaksFormat = null;
    MaterialProperty MaskPow = null;
    //Material
    MaterialEditor m_MaterialEditor;

    public void FindProperties(MaterialProperty[] props)
    {
        //Enum
        blendMode = FindProperty("_BlendMode", props);
        cullMode = FindProperty("_CullMode", props);
        emissMode = FindProperty("_EMISSFORMAT", props);
        emissMode3 = FindProperty("_EMISSFORMAT3", props);
        //base
        baseMap = FindProperty("_BaseMap", props);
        baseColor = FindProperty("_BaseColor", props);
        baseMap3 = FindProperty("_BaseMap3", props);
        baseColor3 = FindProperty("_BaseColor3", props);
        //normal
        normalMap = FindProperty("_NormalMap", props);
        normalScale = FindProperty("_NormalScale", props);
        normalMap3 = FindProperty("_NormalMap3", props);
        normalScale3 = FindProperty("_NormalScale3", props);
        //mase
        maseMap = FindProperty("_MASEMap", props);
        emissMap = FindProperty("_EmissionMap", props);
        emissiveColor = FindProperty("_EmissionColor", props);
        smoothnessMin = FindProperty("_SmoothnessMin", props);
        smoothnessMax = FindProperty("_SmoothnessMax", props);
        metallic = FindProperty("_Metallic", props);
        AOStrength = FindProperty("_AOStrength", props);
        maseMap3 = FindProperty("_MASEMap3", props);
        emissMap3 = FindProperty("_EmissionMap3", props);
        emissiveColor3 = FindProperty("_EmissionColor3", props);
        smoothnessMin3 = FindProperty("_SmoothnessMin3", props);
        smoothnessMax3 = FindProperty("_SmoothnessMax3", props);
        metallic3 = FindProperty("_Metallic3", props);
        AOStrength3 = FindProperty("_AOStrength3", props);
        //Blend Mode
        BlendMaskMap = FindProperty("_MaskMap", props);
        MaksFormat = FindProperty("_MASKFORMAT", props);
        MaskPow = FindProperty("_MaskPow", props);
        //
        cutoff = FindProperty("_Cutoff", props);
        QueueOffset = FindProperty("_QueueOffset", props);
        transparentZWrite = FindProperty("_TransparentZWrite", props);
    }

    public override void OnGUI(MaterialEditor materialEditor, MaterialProperty[] props)
    {
        m_MaterialEditor = materialEditor;
        Material material = materialEditor.target as Material;

        //material.doubleSidedGI = (RenderFace)cullingProp.floatValue != RenderFace.Front;
        // material.doubleSidedGI = true;

        FindProperties(props);
        RenderMode(material);
        ShaderPropertiesGUI(material);

        //override  
        EditorGUILayout.Space(10);
        // m_MaterialEditor.RenderQueueField();     渲染队列 关键字定义
        // m_MaterialEditor.DoubleSidedGIField();   DoubleSidedGI 双面flase 单面 true
        m_MaterialEditor.EnableInstancingField();

        Queue(material);

        // materialEditor.TextureScaleOffsetProperty(props);

    }

    void RenderMode(Material material)
    {
        SetupMaterialWithBlendMode(material, (BlendMode)blendMode.floatValue);

        SetupMaterialWithCullMode(material, (CullMode)cullMode.floatValue);

        SetupMaterialWithemissMode(material, (EmissFormat)emissMode.floatValue);
        SetupMaterialWithemissMode3(material, (EmissFormat3)emissMode3.floatValue);

    }

    public void SetupMaterialWithBlendMode(Material material, BlendMode blendMode)
    {
        switch (blendMode)
        {
            case BlendMode.Opaque:
                material.SetOverrideTag("RenderType", "Opaque");
                SetMaterialKeyword(material, "_ALPHATEST_ON", false);
                SetMaterialKeyword(material, "_ALPHAPREMULTIPLY_ON", false);
                material.SetInt("_ZWrite", 1);
                material.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.One);
                material.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.Zero);
                material.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Geometry;
                break;
            case BlendMode.Cutout:
                material.SetOverrideTag("RenderType", "TransparentCutout");

                SetMaterialKeyword(material, "_ALPHATEST_ON", true);
                SetMaterialKeyword(material, "_ALPHAPREMULTIPLY_ON", false);
                material.SetInt("_ZWrite", 1);
                material.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.One);
                material.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.Zero);
                material.renderQueue = (int)UnityEngine.Rendering.RenderQueue.AlphaTest;
                break;
            case BlendMode.Transparent:
                material.SetOverrideTag("RenderType", "Transparent");
                SetMaterialKeyword(material, "_ALPHATEST_ON", false);
                SetMaterialKeyword(material, "_ALPHAPREMULTIPLY_ON", false);
                material.SetInt("_ZWrite", (int)transparentZWrite.floatValue);
                material.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.SrcAlpha);
                material.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha);
                material.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Transparent;
                break;
            case BlendMode.PreMultiply:
                material.SetOverrideTag("RenderType", "Transparent");
                SetMaterialKeyword(material, "_ALPHATEST_ON", false);
                SetMaterialKeyword(material, "_ALPHAPREMULTIPLY_ON", true);
                material.SetInt("_ZWrite", (int)transparentZWrite.floatValue);
                material.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.One);
                material.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha);
                material.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Transparent;
                break;
        }
    }

    public void SetupMaterialWithCullMode(Material material, CullMode cullMode)
    {
        switch (cullMode)
        {
            case CullMode.Back:
                material.SetInt("_Cull", (int)UnityEngine.Rendering.CullMode.Back);
                break;
            case CullMode.Front:
                material.SetInt("_Cull", (int)UnityEngine.Rendering.CullMode.Front);
                break;
            case CullMode.Double:
                material.SetInt("_Cull", (int)UnityEngine.Rendering.CullMode.Off);
                break;
        }
    }

    public void SetupMaterialWithemissMode(Material material, EmissFormat emissMode)
    {
        switch (emissMode)
        {
            case EmissFormat.Map:
                SetMaterialKeyword(material, "_EMISSFORMAT_MASK", false);
                SetMaterialKeyword(material, "_EMISSFORMAT_MAP", true);
                break;
            case EmissFormat.Mask:
                SetMaterialKeyword(material, "_EMISSFORMAT_MAP", false);
                SetMaterialKeyword(material, "_EMISSFORMAT_MASK", true);
                break;
        }
    }
    public void SetupMaterialWithemissMode3(Material material, EmissFormat3 emissMode)
    {
        switch (emissMode)
        {
            case EmissFormat3.Map:
                SetMaterialKeyword(material, "_EMISSFORMAT3_MASK", false);
                SetMaterialKeyword(material, "_EMISSFORMAT3_MAP", true);
                break;
            case EmissFormat3.Mask:
                SetMaterialKeyword(material, "_EMISSFORMAT3_MAP", false);
                SetMaterialKeyword(material, "_EMISSFORMAT3_MASK", true);
                break;
        }
    }

    const int indent = 1;
    public void ShaderPropertiesGUI(Material material)
    {
        //混合模式
        BlendModePopup();

        if ((BlendMode)blendMode.floatValue == BlendMode.Cutout)
        {
            m_MaterialEditor.ShaderProperty(cutoff, "Cutoff", indent);
        }
        else if ((BlendMode)blendMode.floatValue == BlendMode.Transparent || (BlendMode)blendMode.floatValue == BlendMode.PreMultiply)
        {
            //深度写入应该关闭
            m_MaterialEditor.ShaderProperty(transparentZWrite, "ZWrite", indent);
        }
        //裁切模型
        CullModePopup(material);

        //行间距
        EditorGUILayout.Space(10);

        //basecolor 
        m_MaterialEditor.TexturePropertySingleLine(Styles.baseMapText, baseMap, baseColor);

        if (material.HasProperty("_MainTex"))
        {
            material.SetTexture("_MainTex", baseMap.textureValue);
            material.SetColor("_Color", baseColor.colorValue);

            var baseMapTiling = baseMap.textureScaleAndOffset;
            material.SetTextureScale("_MainTex", new Vector2(baseMapTiling.x, baseMapTiling.y));
            material.SetTextureOffset("_MainTex", new Vector2(baseMapTiling.z, baseMapTiling.w));
        }

        //normal
        if (normalMap.textureValue != null)
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.normalMapText, normalMap, normalScale);
            //keyworld true
            SetMaterialKeyword(material, "_NORMALMAP", normalMap.textureValue != null);
        }
        else
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.normalMapText, normalMap);
            SetMaterialKeyword(material, "_NORMALMAP", false);
        }

        //MSAE 
        // EditorGUILayout.Space();
        m_MaterialEditor.TexturePropertySingleLine(Styles.msaeMapTex, maseMap);
        m_MaterialEditor.ShaderProperty(metallic, "Metallic");

        if (maseMap.textureValue != null)
        {
            float sMin = smoothnessMin.floatValue;
            float sMax = smoothnessMax.floatValue;
            EditorGUI.BeginChangeCheck();
            EditorGUILayout.MinMaxSlider(Styles.smoothnessRemapText, ref sMin, ref sMax, 0.0f, 1.0f);

            if (EditorGUI.EndChangeCheck())
            {
                smoothnessMin.floatValue = sMin;
                smoothnessMax.floatValue = sMax;
            }
            m_MaterialEditor.ShaderProperty(AOStrength, "AoStrength");
        }
        else
        {
            m_MaterialEditor.ShaderProperty(smoothnessMax, "Smoothness");
            smoothnessMin.floatValue = 0;
            AOStrength.floatValue = 1;
        }
        //keyworld
        SetMaterialKeyword(material, "_MSAEMAP", maseMap.textureValue != null);

        EditorGUILayout.Space();

        // Emission 
        EmissModePopup();
        if ((EmissFormat)emissMode.floatValue == EmissFormat.Map)
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.emissMapTex, emissMap);
            SetMaterialKeyword(material, "_EMISSFORMAT_MASK", false);
            SetMaterialKeyword(material, "_EMISSFORMAT_MAP", true);
        }
        else
        {
            SetMaterialKeyword(material, "_EMISSFORMAT_MASK", true);
            SetMaterialKeyword(material, "_EMISSFORMAT_MAP", false);
        }

        m_MaterialEditor.ShaderProperty(this.emissiveColor, "Emisscolor");

        SetMaterialKeyword(material, "_EMISSION", emissiveColor.colorValue.maxColorComponent > 0.0);

        if (emissiveColor.colorValue.maxColorComponent > 0.0)
        {
            EditorGUI.BeginChangeCheck();
            // m_MaterialEditor.LightmapEmissionProperty(indent);
            material.globalIlluminationFlags = MaterialGlobalIlluminationFlags.BakedEmissive;

            if (EditorGUI.EndChangeCheck())
            {
                material.globalIlluminationFlags &= ~MaterialGlobalIlluminationFlags.EmissiveIsBlack;
            }
        }
        // tilling and offset
        EditorGUILayout.Space(10);
        m_MaterialEditor.TextureScaleOffsetProperty(baseMap);

        //basecolor Blend
        EditorGUILayout.Space(10);
        m_MaterialEditor.TexturePropertySingleLine(Styles.baseMapText3, baseMap3, baseColor3);
        var baseMapTiling3 = baseMap3.textureScaleAndOffset;
        material.SetTextureScale("_BaseMap3", new Vector2(baseMapTiling3.x, baseMapTiling3.y));
        material.SetTextureOffset("_BaseMap3", new Vector2(baseMapTiling3.z, baseMapTiling3.w));
        //normal
        if (normalMap3.textureValue != null)
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.normalMapText3, normalMap3, normalScale3);
            //keyworld true
            SetMaterialKeyword(material, "_NORMALMAP3", normalMap3.textureValue != null);
        }
        else
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.normalMapText3, normalMap3);
            SetMaterialKeyword(material, "_NORMALMAP3", false);
        }

        //MSAE 
        m_MaterialEditor.TexturePropertySingleLine(Styles.msaeMapTex3, maseMap3);
        m_MaterialEditor.ShaderProperty(metallic3, "Metallic");

        if (maseMap3.textureValue != null)
        {
            float sMin3 = smoothnessMin3.floatValue;
            float sMax3 = smoothnessMax3.floatValue;
            EditorGUI.BeginChangeCheck();
            EditorGUILayout.MinMaxSlider(Styles.smoothnessRemapText3, ref sMin3, ref sMax3, 0.0f, 1.0f);

            if (EditorGUI.EndChangeCheck())
            {
                smoothnessMin3.floatValue = sMin3;
                smoothnessMax3.floatValue = sMax3;
            }
            m_MaterialEditor.ShaderProperty(AOStrength3, "AoStrength");
        }
        else
        {
            m_MaterialEditor.ShaderProperty(smoothnessMax3, "Smoothness");
            smoothnessMin3.floatValue = 0;
            AOStrength3.floatValue = 1;
        }
        //keyworld
        SetMaterialKeyword(material, "_MSAEMAP3", maseMap3.textureValue != null);

        EditorGUILayout.Space();
        // Emission 
        EmissModePopup3();
        if ((EmissFormat3)emissMode3.floatValue == EmissFormat3.Map)
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.emissMapTex3, emissMap3);
            SetMaterialKeyword(material, "_EMISSFORMAT3_MASK", false);
            SetMaterialKeyword(material, "_EMISSFORMAT3_MAP", true);
        }
        else
        {
            SetMaterialKeyword(material, "_EMISSFORMAT3_MASK", true);
            SetMaterialKeyword(material, "_EMISSFORMAT3_MAP", false);
        }

        m_MaterialEditor.ShaderProperty(this.emissiveColor3, "Emisscolor");

        SetMaterialKeyword(material, "_EMISSION3", emissiveColor3.colorValue.maxColorComponent > 0.0);

        if (emissiveColor3.colorValue.maxColorComponent > 0.0)
        {
            EditorGUI.BeginChangeCheck();
            // m_MaterialEditor.LightmapEmissionProperty(indent);
            material.globalIlluminationFlags = MaterialGlobalIlluminationFlags.BakedEmissive;

            if (EditorGUI.EndChangeCheck())
            {
                material.globalIlluminationFlags &= ~MaterialGlobalIlluminationFlags.EmissiveIsBlack;
            }
        }
        EditorGUILayout.Space(10);
        m_MaterialEditor.TextureScaleOffsetProperty(baseMap3);

        //Blend
        EditorGUILayout.Space(10);
        m_MaterialEditor.ShaderProperty(MaksFormat, "MaksFormat");

        if (MaksFormat.floatValue != 3)
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.maskMapTex, BlendMaskMap, MaskPow);
            //uv
            var MaksMapTiling = BlendMaskMap.textureScaleAndOffset;
            material.SetTextureScale("_MaskMap", new Vector2(MaksMapTiling.x, MaksMapTiling.y));
            material.SetTextureOffset("_MaskMap", new Vector2(MaksMapTiling.z, MaksMapTiling.w));
            m_MaterialEditor.TextureScaleOffsetProperty(BlendMaskMap);
        }
        else
        {
            m_MaterialEditor.ShaderProperty(MaskPow, "MaskPow");
        }


        // m_MaterialEditor.ShaderProperty(sssToggle, "SSS");
        // if (sssToggle.floatValue != 0)
        // {
        //     m_MaterialEditor.ShaderProperty(sssColor, "SSS颜色", indent);
        // }

        // EditorGUILayout.Space(10);
        // m_MaterialEditor.ShaderProperty(this.rimColor, "边缘光颜色");
        // m_MaterialEditor.ShaderProperty(rimPower, "边缘光范围");
        // Color rimColor = this.rimColor.colorValue;
        // rimColor.a = rimPower.floatValue;
        // this.rimColor.colorValue = rimColor;
        // material.SetMaterialKeyword("_RIM", rimColor.maxColorComponent >= 0.04f);
    }

    void BlendModePopup()
    {
        EditorGUI.showMixedValue = blendMode.hasMixedValue;
        var mode = (BlendMode)blendMode.floatValue;

        EditorGUI.BeginChangeCheck();
        mode = (BlendMode)EditorGUILayout.Popup(Styles.renderingMode, (int)mode, Styles.blendNames);

        if (EditorGUI.EndChangeCheck())
        {
            m_MaterialEditor.RegisterPropertyChangeUndo("Rendering Mode");
            blendMode.floatValue = (float)mode;
        }

        EditorGUI.showMixedValue = false;
    }


    void CullModePopup(Material material)
    {
        EditorGUI.showMixedValue = cullMode.hasMixedValue;

        var mode = (CullMode)cullMode.floatValue;

        EditorGUI.BeginChangeCheck();
        mode = (CullMode)EditorGUILayout.Popup(Styles.cullingMode, (int)mode, Styles.cullNames);

        if (EditorGUI.EndChangeCheck())
        {
            m_MaterialEditor.RegisterPropertyChangeUndo("Culling Mode");
            cullMode.floatValue = (float)mode;
            //doubleSidedGI
            material.doubleSidedGI = (CullMode)cullMode.floatValue != CullMode.Double;
            // Debug.Log(material.doubleSidedGI);
        }
        EditorGUI.showMixedValue = false;
    }

    void EmissModePopup()
    {
        EditorGUI.showMixedValue = emissMode.hasMixedValue;

        var mode = (EmissFormat)emissMode.floatValue;

        EditorGUI.BeginChangeCheck();
        mode = (EmissFormat)EditorGUILayout.Popup(Styles.emissformat, (int)mode, Styles.emissNames);

        if (EditorGUI.EndChangeCheck())
        {
            m_MaterialEditor.RegisterPropertyChangeUndo("Emiss Format");

            emissMode.floatValue = (float)mode;
        }
        EditorGUI.showMixedValue = false;
    }
    void EmissModePopup3()
    {
        EditorGUI.showMixedValue = emissMode3.hasMixedValue;

        var mode3 = (EmissFormat3)emissMode3.floatValue;

        EditorGUI.BeginChangeCheck();
        mode3 = (EmissFormat3)EditorGUILayout.Popup(Styles.emissformat3, (int)mode3, Styles.emissNames3);

        if (EditorGUI.EndChangeCheck())
        {
            m_MaterialEditor.RegisterPropertyChangeUndo("Emiss Format");

            emissMode3.floatValue = (float)mode3;
        }
        EditorGUI.showMixedValue = false;
    }

    void Queue(Material material)
    {
        //Render Queue
        EditorGUI.showMixedValue = QueueOffset.hasMixedValue;
        EditorGUI.BeginChangeCheck();
        int mode = EditorGUILayout.IntSlider(Styles.queueSlider, (int)QueueOffset.floatValue, -50, 50);
        if (EditorGUI.EndChangeCheck())
        {
            QueueOffset.floatValue = (int)mode;
        }
        EditorGUI.showMixedValue = false;
        material.renderQueue += material.HasProperty("_QueueOffset") ? (int)material.GetFloat("_QueueOffset") : 0;
        //debug queue
        // Debug.Log(material.renderQueue);
    }

    public void SetMaterialKeyword(Material material, string keyWord, bool toggle)
    {
        if (toggle)
            material.EnableKeyword(keyWord);
        else
            material.DisableKeyword(keyWord);
    }

}