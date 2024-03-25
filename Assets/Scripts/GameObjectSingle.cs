using System;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;
using Object = UnityEngine.Object;

public static class GameObjectSingle
{
    static readonly Dictionary<string, GameObject> Objects = new Dictionary<string, GameObject>();

    public static GameObject GetGameObject(string key)
    {
#if UNITY_EDITOR
        if (playMode == PlayModeStateChange.ExitingPlayMode)
        {
            return null;
        }
#endif
        if (!Objects.TryGetValue(key, out var gameObject) || gameObject == null)
        {
#if UNITY_EDITOR
            gameObject = GameObject.Find(key);
            if (gameObject == null)
#endif
            {
                gameObject = new GameObject(key);

            }
#if UNITY_EDITOR
            gameObject.hideFlags = HideFlags.DontSaveInBuild;
            switch (playMode)
            {
                case PlayModeStateChange.ExitingEditMode:
                case PlayModeStateChange.EnteredPlayMode:
                    Object.DontDestroyOnLoad(gameObject);
                    break;
            }
#else
            Object.DontDestroyOnLoad(gameObject);
#endif
            Objects[key] = gameObject;
        }

        return gameObject;
    }
#if UNITY_EDITOR
    static GameObjectSingle()
    {
        EditorApplication.playModeStateChanged += change;
    }

    static PlayModeStateChange playMode;
    static void change(PlayModeStateChange obj)
    {
         switch (obj)
         {
             case PlayModeStateChange.EnteredEditMode:
             case PlayModeStateChange.ExitingEditMode:
                 foreach (var it in Objects)
                 {
                    var gameObject = it.Value;
// #if UNITY_EDITOR
//                     Object.DestroyImmediate(gameObject);
// #else
//                     Object.Destroy(gameObject);
// #endif
                 }
                 Objects.Clear();
                 break;
         }

        playMode = obj;
    }
#endif
}